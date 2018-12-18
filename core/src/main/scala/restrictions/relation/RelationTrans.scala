package restrictions.relation

import cats.implicits._
import cats.{Applicative, Apply, Monoid, MonoidK, Semigroup}
import restrictions.relation.ast._

import scala.language.implicitConversions

trait RelationTrans[K,I,O] extends (RelationDSL[K,I] => RelationDSL[K,O]) {
  def <<[X](g: RelationTrans[K,X,I]): RelationTrans[K,X,O] =
    i => apply(g(i))

  def >>[X](g: RelationTrans[K,O,X]): RelationTrans[K,I,X] =
    g << this

  def filter(predicate: O => Boolean): RelationTrans[K, I,O] =
    i => this.apply(i) filter predicate
}

object RelationTrans {
  implicit def toRelationTrans[K,A,B](f: RelationDSL[K,A] => RelationDSL[K,B]): RelationTrans[K,A,B] =
    (v1: RelationDSL[K,A]) => f.apply(v1)

  def identity[K, I]: RelationTrans[K, I, I] =
    i => i

  def const[K: Monoid,I,O](x: O): RelationTrans[K,I,O] =
    Applicative[RelationTrans[K,I,?]].pure(x)

  implicit def RelationTransApply[K, I]: Apply[RelationTrans[K, I, ?]] = new RelationTransApply[K, I] { }
  implicit def RelationTransApplicative[K: Monoid, I]: Applicative[RelationTrans[K, I, ?]] = new RelationTransApplicative[K, I] { }
  implicit def RelationTransMonoidK[K, I]: MonoidK[RelationTrans[K,I,?]] = new RelationTransMonoidK[K,I] {}
  implicit def RelationTransSemigroup[K,I,O: Semigroup]: Semigroup[RelationTrans[K,I,O]] = new RelationTransSemigroup[K,I,O] {}

  trait RelationTransApply[K,I] extends Apply[RelationTrans[K, I, ?]] {
    override def ap[A, B](ff: RelationTrans[K, I, A => B])(fa: RelationTrans[K, I, A]): RelationTrans[K, I, B] =
      i => Apply[RelationDSL[K,?]].ap (ff(i)) (fa(i))

    override def map[A, B](fa: RelationTrans[K, I, A])(f: A => B): RelationTrans[K, I, B] =
      i => fa(i).map(f)
  }

  class RelationTransApplicative[K: Monoid, I] extends Applicative[RelationTrans[K,I,?]] with RelationTransApply[K,I] {
    override def pure[A](x: A): RelationTrans[K, I, A] =
      _ => Applicative[RelationDSL[K,?]].pure(x)
  }

  class RelationTransMonoidK[K,I] extends MonoidK[RelationTrans[K,I,?]] {
    override def empty[A]: RelationTrans[K, I, A] =
      _ => MonoidK[RelationDSL[K,?]].empty[A]

    override def combineK[A](x: RelationTrans[K, I, A], y: RelationTrans[K, I, A]): RelationTrans[K, I, A] =
      i => x(i) <+> y(i)
  }

  class RelationTransSemigroup[K,I,O: Semigroup] extends Semigroup[RelationTrans[K,I,O]] {
    override def combine(x: RelationTrans[K, I, O], y: RelationTrans[K, I, O]): RelationTrans[K, I, O] =
      i => x(i) |+| y(i)
  }
}