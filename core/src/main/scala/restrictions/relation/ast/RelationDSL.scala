package restrictions.relation.ast

import cats.implicits._
import cats.data.Ior
import cats.{Applicative, Apply, Monoid, MonoidK, Semigroup}
import restrictions._
import restrictions.stream.ast.{Changelog, StreamDSL}

import scala.language.higherKinds


sealed trait RelationDSL[K, A] {
  def map[B](f: A => B): RelationDSL[K,B] = Map(this, f)
  def filter(f: A => Boolean): RelationDSL[K, A] = Filter(this, f)
  def join[B](right: RelationDSL[K,B]): RelationDSL[K, (A, B)] = InnerJoin(this, right)
  def joinForeign[B](right: RelationDSL[A, B]): RelationDSL[K, B] = JoinValue(this, right)
  def outerJoin[B](right: RelationDSL[K,B]): RelationDSL[K, Ior[A, B]] = OuterJoin(this, right)
  def zip[B](right: StreamDSL[B]): RelationDSL[K, (A, B)] = Zip(this, right)
  def changelog: StreamDSL[RowUpdate[K, A]] = Changelog(this)
}

case class ToRelation[K,A](stream: StreamDSL[RowUpdate[K,A]]) extends RelationDSL[K,A]
case class Map[K,A,B](relation: RelationDSL[K,A], f: A => B) extends RelationDSL[K,B]
case class Filter[K,A](relation: RelationDSL[K,A], predicate: A => Boolean) extends RelationDSL[K, A]
case class InnerJoin[K,A,B](left: RelationDSL[K,A], right: RelationDSL[K,B]) extends RelationDSL[K, (A,B)]
case class OuterJoin[K,A,B](left: RelationDSL[K,A], right: RelationDSL[K,B]) extends RelationDSL[K, A Ior B]
case class Zip[K,L,R](relation: RelationDSL[K,L], a: StreamDSL[R]) extends RelationDSL[K,(L,R)]
case class JoinValue[Kj, K, V](left: RelationDSL[K, Kj], right: RelationDSL[Kj, V]) extends RelationDSL[K, V]

object Never {
  def unapply[K,A](arg: RelationDSL[K,A]): Boolean = arg match {
    case ToRelation(restrictions.stream.ast.Never()) => true
    case _ => false
  }

  def apply[K,A](): RelationDSL[K,A] =
    ToRelation(restrictions.stream.ast.Never())
}

trait RelationDSLInstances {
  implicit def RelationDSLApply[K]: Apply[RelationDSL[K, ?]] = new RelationApply[K] {}
  implicit def RelationDSLApplicative[K: Monoid]: Applicative[RelationDSL[K, ?]] = new RelationApplicative[K] {}
  implicit def RelationDSLSemigroup[K, A: Semigroup]: Semigroup[RelationDSL[K, A]] = new RelationSemigroup[K, A] {}
  implicit def RelationDSLMonoidK[K]: MonoidK[RelationDSL[K, ?]] = new RelationMonoidK[K] {}
}

trait RelationApply[K] extends Apply[RelationDSL[K,?]] {
  override def map[A, B](fa: RelationDSL[K, A])(f: A => B): RelationDSL[K, B] =
    fa.map(f)

  override def ap[A, B](ff: RelationDSL[K, A => B])(fa: RelationDSL[K, A]): RelationDSL[K, B] = {
    val apply: NamedLambda[(A=>B,A), B] = (t: (A=>B,A)) => t._1.apply(t._2)
    ff join fa map apply
  }
}

class RelationApplicative[K: Monoid] extends Applicative[RelationDSL[K, ?]] with RelationApply[K] {
  override def pure[A](x: A): RelationDSL[K, A] =
    Applicative[StreamDSL].pure(RowUpdate(Monoid[K].empty, x)).toRelation
}

class RelationMonoidK[K] extends MonoidK[RelationDSL[K,?]] {
  override def empty[A]: RelationDSL[K, A] =
    MonoidK[StreamDSL].empty[RowUpdate[K,A]].toRelation

  override def combineK[A](x: RelationDSL[K, A], y: RelationDSL[K, A]): RelationDSL[K, A] =
    (x.changelog <+> y.changelog).toRelation
}

class RelationSemigroup[K, A: Semigroup] extends Semigroup[RelationDSL[K,A]] {
  override def combine(x: RelationDSL[K, A], y: RelationDSL[K, A]): RelationDSL[K, A] = {
    val semigroupCombine = { a: A Ior A => a.fold(id, id, Semigroup[A].combine) }.name
    x outerJoin y map semigroupCombine
  }
}