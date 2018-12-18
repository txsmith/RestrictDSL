package restrictions.stream.ast

import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.{Applicative, Monoid, MonoidK}
import restrictions._
import restrictions.relation.ast.{RelationDSL, RowUpdate, ToRelation}
import restrictions.stream.PrettyPrinters

import scala.collection.immutable
import scala.language.implicitConversions

sealed trait StreamDSL[A] {
  def zip[B](right: StreamDSL[B]): StreamDSL[(A,B)] = Zip(this, right)
  def map[B](f: A => B): StreamDSL[B] = Map(this, f)
  def mapConcat[B](f: A => List[B]): StreamDSL[B] = MapConcat(this, f)
  def filter(f: A => Boolean): StreamDSL[A] = Filter(this,f)
  def merge(s: StreamDSL[A]): StreamDSL[A] = Merge(this,s)

  def prettyPrint: String = PrettyPrinters.PrettyPrinter.pretty(this)
  def printAST: String = PrettyPrinters.ASTPrinter.pretty(this)
  def printHTML: String = PrettyPrinters.HTMLPrinter.pretty(this)
}

case class Never[A]() extends StreamDSL[A]
case class Now[A](value: A) extends StreamDSL[A]
case class Named[A](name: String, a: StreamDSL[A]) extends StreamDSL[A]
case class FromAkka[A](akka: Source[A, NotUsed]) extends StreamDSL[A] {
  override def toString = "<akka source>"
}
case class FromSeq[A](seq: scala.collection.immutable.Seq[A]) extends StreamDSL[A] {
  override def toString = s"Seq(${seq.mkString(", ")})"
}

case class Zip[L,R](left: StreamDSL[L], right: StreamDSL[R]) extends StreamDSL[(L,R)]
case class Lookup[K,L,R](a: StreamDSL[(K,L)], relation: RelationDSL[K,R]) extends StreamDSL[(K,L,R)]
case class Merge[A](left: StreamDSL[A], right: StreamDSL[A]) extends StreamDSL[A]
case class Map[A,B](a: StreamDSL[A], f: A => B) extends StreamDSL[B]
case class MapConcat[A,B](a: StreamDSL[A], f: A => List[B]) extends StreamDSL[B]
case class Filter[A](a: StreamDSL[A], f: A => Boolean) extends StreamDSL[A]
case class Changelog[K,A](relation: RelationDSL[K,A]) extends StreamDSL[RowUpdate[K,A]]

trait StreamDSLFunctions {
  def merge[A](xs: StreamDSL[A], ys: StreamDSL[A]): StreamDSL[A] = Merge(xs,ys)
  def never[A]: StreamDSL[A] = MonoidK[StreamDSL].empty
  def now[A](value: A): StreamDSL[A] = Now(value)
  def fromAkka[A](stream: Source[A, NotUsed]): StreamDSL[A] = FromAkka(stream)
  def fromSeq[A](seq: immutable.Seq[A]): StreamDSL[A] = FromSeq(seq)
  def named[A](name: String, s: StreamDSL[A]): StreamDSL[A] = Named(name, s)

  implicit def toRelationOps[K,A](s: StreamDSL[RowUpdate[K,A]]): ToRelationOps[K, A] =
    new ToRelationOps[K,A](s)

  implicit def fromAkkaOps[A](s: Source[A, NotUsed]): FromAkkaOps[A] =
    new FromAkkaOps[A](s)
}

class ToRelationOps[K,A](s: StreamDSL[RowUpdate[K, A]]) {
  def toRelation: RelationDSL[K, A] = ToRelation(s)
}

class FromAkkaOps[A](s: Source[A, NotUsed]) {
  def fromAkka: StreamDSL[A] = FromAkka(s)
}

trait StreamDSLInstances {
  implicit val streamApplicative: Applicative[StreamDSL] = new Applicative[StreamDSL] {
    override def pure[A](x: A): StreamDSL[A] =
      Now(x)

    override def map[A, B](fa: StreamDSL[A])(f: A => B): StreamDSL[B] =
      Map(fa, f)

    override def map2[A, B, Z](fa: StreamDSL[A], fb: StreamDSL[B])(f: (A, B) => Z) = {
      val applyTuple: NamedLambda[(A, B), Z] = f.tupled.name("{(a,b) => f(a,b)}")
      Map(Zip(fa, fb), applyTuple)
    }

    override def ap[A, B](ff: StreamDSL[A => B])(fa: StreamDSL[A]): StreamDSL[B] = {
      val apply = ((t: (A => B, A)) => t._1.apply(t._2)).name("{(f,x) => f(x)}")
      Map(Zip(ff, fa), apply)
    }
  }

  implicit val streamMonoidK: MonoidK[StreamDSL] = new MonoidK[StreamDSL] {
    override def empty[A]: StreamDSL[A] =
      Never[A]()

    override def combineK[A](x: StreamDSL[A], y: StreamDSL[A]): StreamDSL[A] =
      x merge y
  }

  implicit def streamMonoid[A]: Monoid[StreamDSL[A]] = streamMonoidK.algebra[A]
}