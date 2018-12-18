package restrictions.ast

import cats.implicits._
import cats.{Applicative, Functor, Traverse}
import restrictions.SourceInfo
import restrictions.domain._
import higherkindness.droste.data.Fix
import higherkindness.droste.util.DefaultTraverse

import scala.language.{higherKinds, implicitConversions}

/**
 * Abstract Syntax Tree for restrict statements.
 * This data type represents restrictions like:
 * {{{
 *   when (wind.velocity >= 20.knots) {
 *     require (vessel.length <= 100.meters)
 *   }
 * }}}
 *
 * Terms like `when`, `require` and `restrict` are represented in RestrictStatement.
 * Conditions like `vessel.length <= 100.meters` are represented by [[Observation]].
 *
 *
 * What's special about this type is that it has holes (the `A` type variable).
 * This is done so we can get multiple different kinds of representations of the
 * same AST without having to duplicate code for each case.
 *
 * Currently we use two representations:
 *  - Fix[RestrictStatementF]
 *    [[RestrictStatement]] is defined this way. Fix[_] makes sure that our AST has
 *    no holes in it. Each hole that would otherwise be an `A` is replaced by the AST
 *    itself, recursively. This gives us a tree where all the nodes are
 *    RestrictStatement values.
 *
 *  - Free[RestrictStatementF, X]
 *    This works like Fix, but instead of forcing each node to be a RestrictStatement,
 *    we may leave some nodes of the tree out and put an X in its place.
 *    This allows us to parse ASTs incrementally, one level at a time.
 *    See RuleParser for more details.
 *
 * If you want to know how Fix and Free work, I strongly recommend starting with this
 * talk by Rob Norris: https://www.youtube.com/watch?v=7xSfLPD6tiQ
 */
sealed abstract class RestrictStatementF[A](override val sourceInfo: SourceInfo) extends WithSourceInfo(sourceInfo)
case class RestrictLocationF[A](location: Location, restrictions: List[A])(implicit s: SourceInfo) extends RestrictStatementF[A](s)
case class RequireF[A](restriction: Observation[Boolean])(implicit s: SourceInfo) extends RestrictStatementF[A](s)
case class WhenF[A](condition: Observation[Boolean], restrictions: List[A])(implicit s: SourceInfo) extends RestrictStatementF[A](s)


/**
 * The rest of this file is just bookkeeping to make sure we can
 * pattern match without Fix getting in the way.
 */
object RestrictLocation {
  def apply(location: Location, restrictions: List[RestrictStatement])(implicit s: SourceInfo): RestrictStatement =
    Fix(RestrictLocationF(location, restrictions))

  def unapply(arg: RestrictStatement): Option[(Location, List[RestrictStatement])] =
    Fix.un[RestrictStatementF](arg) match {
      case RestrictLocationF(loc, rules) => Some((loc, rules))
      case _ => None
    }
}

object Require {
  def apply(restriction: Observation[Boolean])(implicit s: SourceInfo): RestrictStatement =
    Fix(RequireF(restriction))

  def unapply(arg: RestrictStatement): Option[Observation[Boolean]] =
    Fix.un[RestrictStatementF](arg) match {
      case RequireF(restriction) => Some(restriction)
      case _ => None
    }
}

object When {
  def apply(condition: Observation[Boolean], restrictions: List[RestrictStatement])(implicit s: SourceInfo): RestrictStatement =
    Fix(WhenF(condition, restrictions))

  def unapply(arg: RestrictStatement): Option[(Observation[Boolean], List[RestrictStatement])] =
    Fix.un[RestrictStatementF](arg) match {
      case WhenF(condition, restrictions) => Some((condition, restrictions))
      case _ => None
    }
}

trait RestrictStatementInstances {
  implicit val treeFunctor: Functor[RestrictStatementF] = new Functor[RestrictStatementF] {
    override def map[A, B](fa: RestrictStatementF[A])(f: A => B): RestrictStatementF[B] = fa match {
      case RestrictLocationF(l, as) => RestrictLocationF(l, as.map(f))
      case WhenF(c, as) => WhenF(c, as.map(f))
      case RequireF(c) => RequireF(c)
    }
  }

  implicit val treeTraverse: Traverse[RestrictStatementF] = new DefaultTraverse[RestrictStatementF] {
    override def traverse[G[_], A, B]
    (fa: RestrictStatementF[A])
    (f: A => G[B])
    (implicit evidence: Applicative[G]): G[RestrictStatementF[B]] = {
      treeFunctor.map(fa)(f) match {
        case RestrictLocationF(loc, as) => as.sequence.map(RestrictLocationF(loc, _))
        case WhenF(c, as) => as.sequence.map(WhenF(c, _))
        case RequireF(restriction) => Applicative[G].pure(RequireF(restriction))
      }
    }
  }
}