package restrictions.ast

import cats.implicits._
import cats.{Applicative, ~>}
import restrictions.SourceInfo
import restrictions.domain._
import restrictions.domain.external._
import org.bitbucket.inkytonik.kiama.output._
import pronto.domain.Berth

import scala.language.higherKinds
import scala.math.Ordering

/**
 * An Observation[A] represents an expression that results in an A at some point in the future.
 * In the restrictions DSL this is used to represent expressions like:
 * {{{
 *   vessel.length <= 200.meters
 *   wind.velocity >= 20.knots
 *   vessel.draught + 50.centimeters <= 19.5.meters + tide.height
 * }}}
 *
 * This is the AST for expressions that get used inside [[RestrictStatement]]s.
 * For these definitions we don't use recursion schemes due to the fact this AST is too complex
 * to be compatible with standard recursion schemes. In particular, having the `A` type variable
 * and having child expressions with different types (like Map) makes things hard.
 *
 * We keep macro-generated source locations in the AST in case you'd ever want to print where an expression comes from.
 */
sealed abstract class Observation[A](override val sourceInfo: SourceInfo) extends WithSourceInfo(sourceInfo) with PrettyExpression {
  def foldApplicative[F[_]: Applicative](f: Observation ~> F): F[A] = Observation.foldApplicative(this, f)
}

/**
 * These classes are inherited to support nice pretty printing of operators.
 */
abstract class BinOp[A](override val op: String,
                        override val priority: Int,
                        override val fixity: Fixity,
                        override val sourceInfo: SourceInfo)
  extends Observation[A](sourceInfo) with PrettyBinaryExpression {
  override val left: Observation[_]
  override val right: Observation[_]
}
abstract class UnaryOp[A](override val op: String,
                          override val priority: Int,
                          override val fixity: Fixity,
                          override val sourceInfo: SourceInfo)
  extends Observation[A](sourceInfo) with PrettyUnaryExpression {
  override val exp: Observation[_]
}

case class Named[A](name: String, observation: Observation[A])(implicit s: SourceInfo) extends Observation[A](s)
case class Void[A]()(implicit s: SourceInfo) extends Observation[A](s)

case class GetVessel()(implicit s: SourceInfo) extends Observation[Vessel](s)
case class GetWind()(implicit s: SourceInfo) extends Observation[Wind](s)
case class GetTide()(implicit s: SourceInfo) extends Observation[Tide](s)
case class GetTidalStream()(implicit s: SourceInfo) extends Observation[TidalStream](s)
case class GetBerth()(implicit s: SourceInfo) extends Observation[Berth](s)
// TODO: make Observations more extensible

case class Equals[A](left: Observation[A], right: Observation[A])(implicit s: SourceInfo) extends BinOp[Boolean]("is", 6, Infix(NonAssoc), s)
case class LessOrEq[A](left: Observation[A], right: Observation[A], ord: Ordering[A])(implicit s: SourceInfo) extends BinOp[Boolean]("<=", 6, Infix(NonAssoc), s)
case class GreaterOrEq[A](left: Observation[A], right: Observation[A], ord: Ordering[A])(implicit s: SourceInfo) extends BinOp[Boolean](">=", 6, Infix(NonAssoc), s)
case class LessThan[A](left: Observation[A], right: Observation[A], ord: Ordering[A])(implicit s: SourceInfo) extends BinOp[Boolean]("<", 6, Infix(NonAssoc), s)
case class GreaterThan[A](left: Observation[A], right: Observation[A], ord: Ordering[A])(implicit s: SourceInfo) extends BinOp[Boolean](">", 6, Infix(NonAssoc), s)
case class Minus[A](left: Observation[A], right: Observation[A], num: Numeric[A])(implicit s: SourceInfo) extends BinOp[A]("-", 4, Infix(LeftAssoc), s)
case class Plus[A](left: Observation[A], right: Observation[A], num: Numeric[A])(implicit s: SourceInfo) extends BinOp[A]("+", 4, Infix(LeftAssoc), s)
case class Times[A,B,R](left: Observation[A], right: Observation[B], instance: SafeMultiply[A,B,R])(implicit s: SourceInfo) extends BinOp[R]("*", 3, Infix(LeftAssoc), s)
case class Divide[A,B,R](left: Observation[A], right: Observation[B], instance: SafeDivide[A,B,R])(implicit s: SourceInfo) extends BinOp[R]("/", 7, Infix(LeftAssoc), s)
case class And(left: Observation[Boolean], right: Observation[Boolean])(implicit s: SourceInfo) extends BinOp[Boolean]("and", 7, Infix(RightAssoc), s)
case class Or(left: Observation[Boolean], right: Observation[Boolean])(implicit s: SourceInfo) extends BinOp[Boolean]("or", 8, Infix(RightAssoc), s)
case class Negate[A](exp: Observation[A], num: Numeric[A])(implicit s: SourceInfo) extends UnaryOp[A]("-", 0, Prefix, s)
case class Not(exp: Observation[Boolean])(implicit s: SourceInfo) extends UnaryOp[Boolean]("not", 0, Prefix, s)
case class Absolute[A](observation: Observation[A], num: Numeric[A])(implicit s: SourceInfo) extends Observation[A](s)
case class Signum[A](observation: Observation[A],num: Numeric[A])(implicit s: SourceInfo) extends Observation[Int](s)
case class Compare[A](left: Observation[A], right: Observation[A], ord: Ordering[A])(implicit s: SourceInfo) extends Observation[Int](s)

/**
 * These definitions make our AST a Free Applicative.
 * Enables us to map any function over multiple [[Observation]]s
 */
case class Constant[A](value: A)(implicit s: SourceInfo) extends Observation[A](s)
case class Map[A,B](observation: Observation[A], f: A => B)(implicit s: SourceInfo) extends Observation[B](s)
case class Ap[A,B](fn: Observation[A => B], a: Observation[A])(implicit s: SourceInfo) extends Observation[B](s)

trait ObservationInstances {
  implicit val observationApplicative: Applicative[Observation] = new Applicative[Observation] {
    override def pure[A](x: A): Observation[A] =
      Constant(x)

    override def ap[A, B](ff: Observation[A => B])(fa: Observation[A]): Observation[B] =
      Ap(ff, fa)

    override def map[A, B](fa: Observation[A])(f: A => B): Observation[B] =
      Map(fa,f)
  }
}

object Observation {
  /**
   * Since Observation[A] is a free applicative, it must hold that we can take any Observation[A], pick an arbitrary
   * Applicative[F], and then fold the observation into a F[A].
   *
   * This function does that for all constructors of Observation that have an obvious mapping to any other applicative.
   * For example, Equals(l, r) can be represented in any applicative by lifting the equality function over `l` and `r`
   * with map2.
   *
   * This is not the case for all constructors though.
   * That's why this fold must be given an `Observation[A] => F[A]` to map these constructors:
   *  - GetWind
   *  - GetTide
   *  - GetVessel
   *  - GetBerth
   *  - Void
   *  - Named
   */
  def foldApplicative[F[_]: Applicative, A](o: Observation[A], f: Observation ~> F): F[A] =
    λ[Observation ~> F] {
      case Constant(x) => Applicative[F].pure(x)
      case Map(obs, g) => Applicative[F].map(foldApplicative(obs, f))(g)
      case Ap(fObs, aObs) => Applicative[F].ap(fObs.foldApplicative(f))(aObs.foldApplicative(f))
      case Equals(l, r) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(_ == _)
      case Compare(l, r, ord) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(ord.compare)
      case LessOrEq(l, r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.lteq)
      case GreaterOrEq(l, r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.gteq)
      case LessThan(l, r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.lt)
      case GreaterThan(l, r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.gt)
      case Plus(l,r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.plus)
      case Minus(l,r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.minus)
      case Absolute(x, instance) => x.foldApplicative(f) map instance.abs
      case Signum(x, instance) => x.foldApplicative(f) map instance.signum
      case Times(l,r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.multiply)
      case Divide(l,r, instance) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(instance.divide)
      case And(l,r) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(_ && _)
      case Or(l,r) => Applicative[F].map2(l.foldApplicative(f), r.foldApplicative(f))(_ && _)
      case Not(x) => x.foldApplicative(f) map (!_)
      case other => f(other)
    }.apply(o)
}

