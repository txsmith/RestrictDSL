package restrictions.stream

import cats.{Applicative, Functor, Monoid, MonoidK}
import restrictions.stream.ast._

import scala.language.implicitConversions

trait StreamTrans[I,O] extends (StreamDSL[I] => StreamDSL[O]) {
  def <<[X](g: StreamTrans[X,I]): StreamTrans[X,O] =
    i => apply(g(i))

  def >>[X](g: StreamTrans[O,X]): StreamTrans[I,X] =
    g << this

  def zip[P](right: StreamTrans[I, P]): StreamTrans[I, (O, P)] =
    i => this.apply(i) zip right(i)

  def filter(predicate: O => Boolean): StreamTrans[I,O] =
    i => this.apply(i) filter predicate

  def merge(right: StreamTrans[I, O]): StreamTrans[I,O] =
    i => this.apply(i) merge right(i)
}

object StreamTrans {
  type >=>[I,O] = StreamTrans[I,O]

  implicit def toStreamTrans[A,B](f: StreamDSL[A] => StreamDSL[B]): StreamTrans[A,B] =
    (v1: StreamDSL[A]) => f.apply(v1)

  def identity[I]: StreamTrans[I, I] =
    i => i

  def const[I,O](x: O): StreamTrans[I,O] =
    _ => now(x)

  def void[I,O]: StreamTrans[I,O] =
    _ => never


  implicit def StreamTransFunctor[I]: Functor[StreamTrans[I, ?]] = new Functor[StreamTrans[I,?]] {
    override def map[A, B](fa: StreamTrans[I, A])(f: A => B): StreamTrans[I, B] =
      i => fa(i).map(f)
  }

  implicit def StreamTransApplicative[I]: Applicative[StreamTrans[I, ?]] = new Applicative[StreamTrans[I, ?]] {
    override def pure[A](x: A): StreamTrans[I, A] =
      const(x)

    override def ap[A, B](ff: StreamTrans[I, A => B])(fa: StreamTrans[I, A]): StreamTrans[I, B] =
      i => Applicative[StreamDSL].ap (ff(i)) (fa(i))
  }

  implicit def StreamTransMonoidK[I]: MonoidK[StreamTrans[I,?]] = new MonoidK[StreamTrans[I,?]] {
    override def empty[A]: StreamTrans[I, A] =
      _ => Never[A]()

    override def combineK[A](x: StreamTrans[I, A], y: StreamTrans[I, A]): StreamTrans[I, A] =
      x merge y
  }

  implicit def StreamTransMonoid[I,O]: Monoid[StreamTrans[I,O]] =
    MonoidK[StreamTrans[I,?]].algebra[O]

}
