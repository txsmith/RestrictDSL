package restrictions.utils

import cats.{Applicative, Functor, Id, ~>}
import scala.language.higherKinds

/**
 * Defines two type classes that do not come with cats: HFunctor and HTraverse.
 * We use this to avoid quite some code duplication on the [[restrictions.domain.WindF]] and
 * [[restrictions.domain.TideF]] types. There, it allows us to have one data type for wind measurements where:
 * - measurements may be in the future (Observation[_])
 * - measurements may be unavailable (Option[_])
 * - measurements may be predicted based on interpolation (Interpolator[_])
 *
 * For a thorough introduction to HFunctors read this article (first half is relevant for this code):
 * https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html
 */
trait HFunctors {
  type Compose[F[_], G[_], A] = F[G[A]]
  type Observation[A] = restrictions.ast.Observation[A]

  /**
   * HFunctor is Functor for types that take a higher-kinded type as argument instead of an ordinary type.
   * I.e.:
   *   `Option[_]` is a Functor because it takes a type as an argument
   *   `WindF[_[_]]` is a HFunctor because it takes a type *constructor* as an argument
   */
  trait HFunctor[T[_[_]]] {
    def map[F[_], G[_]](f: F ~> G)(t: T[F]): T[G]
  }
  object HFunctor {
    def apply[T[_[_]]](implicit instance: HFunctor[T]): HFunctor[T] = instance
  }

  /**
   * HTraverse is Traverse for types that take a type constructor (F[_]) as argument instead of a normal type.
   * It is to Traverse as HFunctor is to Functor.
   *
   * Ordinary Traverse[T] lets you use any effectful function `A => Effect[B]` to turn a `T[A]` into an `Effect[T[B]]`.
   * HTraverse lets you use any effectful transformation: `F[A] => Effect[G[A]]` to turn `T[F]` into an `Effect[T[G]]`.
   */
  trait HTraverse[T[_[_]]] extends HFunctor[T] {

    def traverse[F[_], G[_], Effect[_]: Applicative](f: F ~> Compose[Effect,G,?])(t: T[F]): Effect[T[G]]

    def sequence[A[_]: Applicative](t: T[A]): A[T[Id]] =
      sequence_[Id, A](t)

    def sequence_[F[_], A[_]: Applicative](t: T[Compose[A,F,?]]): A[T[F]] =
      traverse[Compose[A,F,?], F, A](id)(t)

    private def id[A[_], F[_]]: Compose[A,F,?] ~> Compose[A,F,?] =
      λ[Compose[A,F,?] ~> Compose[A,F,?]] { x => x }
  }

  object HTraverse {
    def apply[T[_[_]]](implicit instance: HTraverse[T]): HTraverse[T] = instance
  }


  /**
   * Extension methods for `_.traverse(h)`, _.map(h) and `_.sequence`.
   */
  implicit class HTraverseOps[F[_], T[_[_]]: HTraverse](t: T[F]) {
    def traverse[G[_], A[_]: Applicative](f: F ~> Compose[A,G,?]): A[T[G]] =
      HTraverse[T].traverse(f)(t)
  }

  implicit class HTraverseSequenceOps[A[_]: Applicative, T[_[_]]: HTraverse](t: T[A]) {
    def sequence: A[T[Id]] = HTraverse[T].sequence(t)
  }

  implicit class HFunctorOps[F[_], T[_[_]]: HFunctor](t: T[F]) {
    def map[G[_]](f: F ~> G): T[G] =
      HFunctor[T].map(f)(t)
  }

  /**
   * If `F[_]` and `G[_]` are both functors, then `F[G[_]]` must also be a functor.
   * The same holds for applicatives.
   *
   * Why are these not in cats?
   */
  implicit def composeFunctor[F[_]: Functor, G[_]: Functor]: Functor[Compose[F, G, ?]] = Functor[F].compose[G]

  implicit def composeApplicative[F[_]: Applicative, G[_]: Applicative]: Applicative[Compose[F, G, ?]] = Applicative[F].compose[G]
}
