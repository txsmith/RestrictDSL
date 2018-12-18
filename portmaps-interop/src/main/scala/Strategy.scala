import cats._
import cats.data.Nested
import cats.free.Free
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.scheme._
import higherkindness.droste.data.{CoattrF, Fix}

import scala.language.{higherKinds, implicitConversions}

/**
 * Minimal library for building rewrite rules as seen in Main.scala.
 * This was built on top of recursion schemes with droste, which allows
 * us create rewrite rules over many kinds of ASTs without needing weird
 * runtime reflection (the reason for Kiamas bugginess)
 */
trait Strategy[F[_]] extends (Fix[F] => Nested[Option, F, Fix[F]]) {
  import Strategy._

  private val embedFix = Fix[F](_)

  def run: Fix[F] => Fix[F] = this.andThen(x => embedFix(x.value.get))
  def runOption: Fix[F] => Option[Fix[F]] = this.andThen(_.value.map(embedFix))

  def attempt: Strategy[F] =
    a => Nested(this.apply(a).value.orElse(Some(Fix.un(a))))


  def <*(strategy: Strategy[F]): Strategy[F] =
    andThen(strategy)

  def andThen(strategy: Strategy[F]): Strategy[F] =
    tree => {
      Nested(this(tree).value >>= (embedFix andThen (strategy(_).value)))
    }

  def +(r: Strategy[F]): Strategy[F] =
    or(r)

  def or(r: Strategy[F]): Strategy[F] =
    tree => Nested(this(tree).value orElse r(tree).value)

  def repeat: Strategy[F] =
    (this andThen (repeat(_))).attempt

  def topdown(implicit evidence: Traverse[F]): Strategy[F] = Strategy[F] {
    ana(coalgebra) andThen anaM(extractFree[F, Unit]) andThen (_.right.toOption.map(Fix.un(_)))
  }

  def bottomup(implicit evidence: Traverse[F]): Strategy[F] = Strategy[F] {
    cata(algebra)
  }

  def coalgebra(implicit evidence: Functor[F]): Coalgebra[Nested[Option, F, ?], Fix[F]] =
    Coalgebra { this(_) }

  def algebra(implicit evidence: Traverse[F]): Algebra[F, Option[F[Fix[F]]]] =
    Algebra {
      _.map(_ >>= (fFixF => this (embedFix(fFixF)).value.map(embedFix))).sequence
    }
}

object Strategy extends BasisImplicit {
  type =?>[A, B] = PartialFunction[A, B]

  def rule[F[_]](f: Fix[F] =?> Fix[F]): Strategy[F] =
    apply(f.andThen(Fix.un(_)).lift)

  def ruleOption[F[_]](f: Fix[F] => Option[Fix[F]]): Strategy[F] =
    apply(f.andThen(_.map(Fix.un(_))))

  def apply[F[_]](f: Fix[F] => Option[F[Fix[F]]]): Strategy[F] =
    tree => Nested(f(tree))

  def run[F[_]](strategy: Strategy[F]): Fix[F] => Fix[F] =
    strategy.run

  def runOption[F[_]](strategy: Strategy[F]): Fix[F] => Option[Fix[F]] =
    strategy.runOption

  def attempt[F[_]](mayFail: Strategy[F]): Strategy[F] =
    mayFail.attempt

  def topdown[F[_] : Traverse](strategy: Strategy[F]): Strategy[F] =
    strategy.topdown

  def bottomup[F[_] : Traverse](strategy: Strategy[F]): Strategy[F] =
    strategy.bottomup

  def repeat[F[_]](strategy: Strategy[F]): Strategy[F] =
    strategy.repeat

  def fail[F[_]]: Strategy[F] =
    Strategy[F] { _ => Option.empty }

  def fail[F[_]](msg: String): Strategy[F] =
    debug[F](msg) andThen fail[F]

  def debug[F[_]]: Strategy[F] =
    tree => debug[F](tree.toString)(tree)

  def debug[F[_]](msg: String): Strategy[F] =
    rule[F] { case x => println(msg); x }

  def extractFree[F[_]: Functor, A]: CoalgebraM[Either[A, +?], F, Free[F,A]] =
    CoalgebraM {
      Project[CoattrF[F, A, ?], Free[F, A]].coalgebra.run.andThen {
        case CoattrF.Pure(a) => Either.left(a)
        case CoattrF.Roll(fa) => Either.right(fa)
      }
    }
}

trait BasisImplicit {
  implicit def drosteBasisForCatsFreeOfUnit[F[_]: Functor]: Basis[Nested[Option, F, ?], Free[F, Unit]] =
    Basis.Default[Nested[Option, F, ?], Free[F, Unit]](
      Algebra[Nested[Option, F, ?], Free[F, Unit]] {
        _.value.fold(Free.pure[F, Unit]({}))(Free.roll)
      },
      Coalgebra {
        _.fold(_ => Nested(Option.empty[F[Free[F,Unit]]]), x => Nested(Option(x)))
      }
    )

  implicit val drosteSolveCatsFreeOfUnit: Basis.Solve.Aux[cats.free.Free[?[_], Unit], Nested[Option, ?[_], ?]] = null
}