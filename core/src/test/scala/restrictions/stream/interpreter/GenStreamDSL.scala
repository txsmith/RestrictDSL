package restrictions.stream.interpreter

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import akka.NotUsed
import akka.stream.scaladsl.Source
import cats.data.Ior
import restrictions._
import restrictions.stream.ast._
import restrictions.relation.ast._

import scala.collection.immutable

/**
  * Generators, Arbitrary, Shrink and Cogen instances for StreamDSL
  * In English: allows us to generate random trees of StreamDSL terms.
  */
object GenStreamDSL extends
  ArbitraryStreamInstances with
  ArbitraryRelationInstances with
  CogenInstances with
  ShrinkInstances {
}
case class Key(key: String) extends AnyVal


/**
  * Arbitrary instances for generating Source[A, NotUsed] and StreamDSL[Any]
  */
trait ArbitraryStreamInstances extends
  ArbitrarySourceInstances with
  FunctionGenerators with
  CogenInstances {

  implicit def intArbitrary: Arbitrary[Int] =
    Arbitrary(Gen.oneOf(1 to 42))

  implicit def streamDSLArbitrary[A: Arbitrary: Cogen]: Arbitrary[StreamDSL[A]] =
    Arbitrary(Gen.sized(StreamGenerators.genStreamDSL[A]))

  object StreamGenerators {
    def genStreamDSL[A: Arbitrary: Cogen](size: Int): Gen[StreamDSL[A]] = size match {
      case 0 => oneOf(nowGen[A], fromSeqGen[A]) // Skip neverGen here because we don't want empty streams
      case n => oneOf(
        lzy(filterGen[A](genStreamDSL[A](n-1))),
        lzy(mapGen[A, A](genStreamDSL[A](n-1))),
        lzy(mapGen[(A, Boolean), A](genTupleStreamDSL[A, Boolean](n-1))),
        lzy(mergeGen[A](genStreamDSL(n/2))))
    }

    def genTupleStreamDSL[A: Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[StreamDSL[(A,B)]] = size match {
      case 0 => oneOf(nowGen[(A,B)], fromSeqGen[(A,B)]) // Skip neverGen here because we don't want empty streams
      case n => oneOf(
        lzy(filterGen[(A,B)](genTupleStreamDSL[A,B](n-1))),
        lzy(mapGen[(A,B), (A,B)](genTupleStreamDSL[A,B](n-1))),
        lzy(mergeGen[(A,B)](genTupleStreamDSL[A,B](n/2))),
        lzy(zipGen[A,B](n/2)))
    }

    def mapGen[X: Cogen, A: Arbitrary](inner: Gen[StreamDSL[X]]): Gen[StreamDSL[A]] = for {
      f <- arbitrary[X => A]
      stream <- inner
    } yield stream map f

    def filterGen[A](inner: Gen[StreamDSL[A]]): Gen[StreamDSL[A]] = for {
      // predicate <- predicateGen
      stream <- inner
      // Use alwaysTrue for now because we don't want to end up with empty streams.
    } yield stream filter alwaysTrue

    def zipGen[A: Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[StreamDSL[(A,B)]] = for {
      left <- genStreamDSL[A](size/2)
      right <- genStreamDSL[B](size/2)
    } yield left zip right

    def mergeGen[A](inner: Gen[StreamDSL[A]]): Gen[StreamDSL[A]] = for {
      left <- inner
      right <- inner
    } yield left merge right

    def neverGen[A] = Gen.const(never[A])
    def nowGen[A: Arbitrary] = arbitrary[A] map now
    def fromSeqGen[A: Arbitrary] =
      Gen.nonEmptyListOf(arbitrary[A]) map(xs => fromSeq(immutable.Seq(xs:_*)))
  }
}


trait ArbitraryRelationInstances extends
  ArbitrarySourceInstances with
  FunctionGenerators with
  CogenInstances {

  implicit val keyArbitrary: Arbitrary[Key] =
    Arbitrary(Gen.oneOf("A", "B", "C", "D") map Key)

  implicit def rowUpdateArbitrary[K: Arbitrary, A: Arbitrary]: Arbitrary[RowUpdate[K,A]] =
    Arbitrary(oneOf(
      arbitrary[K] map (Delete(_)),
      arbitrary[(K, A)] map (t => Update(t._1, t._2))
    ))

  implicit def relationDSLArbitrary[K: Arbitrary, A: Arbitrary: Cogen]: Arbitrary[RelationDSL[K, A]] =
    Arbitrary(Gen.sized(RelationGenerators.genRelationDSL[K, A]))

  object RelationGenerators {
    def genRelationDSL[K: Arbitrary, A: Arbitrary: Cogen](size: Int): Gen[RelationDSL[K, A]] = size match {
      case 0 => toRelationGen[K,A]
      case n => oneOf(
        lzy(mapGen[K,A,A](genRelationDSL[K,A](n-1))),
        lzy(mapGen[K,(A,Boolean),A](genTupleRelationDSL[K,A,Boolean](n-1))),
        lzy(mapGen[K, A Ior Boolean, A](genIorRelationDSL[K,A,Boolean](n-1))),
        lzy(filterGen[K,A](genRelationDSL[K,A](n-1))))
    }

    def genTupleRelationDSL[K: Arbitrary, A: Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[RelationDSL[K, (A,B)]] =
      size match {
        case 0 => toRelationGen[K, (A,B)]
        case n => oneOf(
          lzy(filterGen[K,(A,B)](genTupleRelationDSL[K,A,B](n-1))),
          lzy(mapGen[K,(A,B),(A,B)](genTupleRelationDSL[K,A,B](n-1))),
          lzy(innerJoinGen[K,A,B](n-1)),
          lzy(zipGen[K,A,B](n-1))
        )
      }

    def genIorRelationDSL[K: Arbitrary, A: Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[RelationDSL[K, A Ior B]] =
      size match {
        case 0 => toRelationGen[K, A Ior B]
        case n => oneOf(
          lzy(filterGen[K, A Ior B](genIorRelationDSL[K,A,B](n-1))),
          lzy(mapGen[K, A Ior B, A Ior B](genIorRelationDSL[K,A,B](n-1))),
          lzy(outerJoinGen[K,A,B](n-1))
        )
      }

    def toRelationGen[K: Arbitrary, A: Arbitrary]: Gen[RelationDSL[K, A]] =
      arbitrary[immutable.Seq[RowUpdate[K, A]]].map(fromSeq(_).toRelation)

    def mapGen[K, X: Cogen, A: Arbitrary](inner: Gen[RelationDSL[K, X]]): Gen[RelationDSL[K, A]] = for {
      f <- arbitrary[X => A]
      stream <- inner
    } yield stream map f

    def filterGen[K,A](inner: Gen[RelationDSL[K, A]]): Gen[RelationDSL[K, A]] = for {
      // predicate <- predicateGen
      stream <- inner
      // Use alwaysTrue for now because we don't want to end up with empty streams.
    } yield stream filter alwaysTrue

    def innerJoinGen[K: Arbitrary, A : Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[RelationDSL[K, (A, B)]] = for {
      left <- genRelationDSL[K, A](size/2)
      right <- genRelationDSL[K, B](size/2)
    } yield left join right

    def outerJoinGen[K: Arbitrary, A : Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[RelationDSL[K, A Ior B]] = for {
      left <- genRelationDSL[K, A](size/2)
      right <- genRelationDSL[K, B](size/2)
    } yield left outerJoin right

    def zipGen[K: Arbitrary, A: Arbitrary: Cogen, B: Arbitrary: Cogen](size: Int): Gen[RelationDSL[K, (A,B)]] =
      for {
        left <- genRelationDSL[K, A](size/2)
        right <- arbitrary[immutable.Seq[B]] map fromSeq
      } yield left zip right
  }
}

trait ArbitrarySourceInstances {
  /**
    * Arbitrary `Source[A, NotUsed]`s are generated from a non-empty list of `A`s
    */
  implicit def sourceArbitrary[A: Arbitrary]: Arbitrary[Source[A, NotUsed]] =
    Arbitrary(
      Gen.nonEmptyListOf(arbitrary[A]) map (Source(_))
    )

  implicit def iorArbitrary[A: Arbitrary, B: Arbitrary]: Arbitrary[A Ior B] =
    Arbitrary(
      arbitrary[Either[A, Either[B, (A,B)]]] map {
        case Left(a) => Ior.left(a)
        case Right(Left(b)) => Ior.right(b)
        case Right(Right((a,b))) => Ior.both(a,b)
      }
    )

  implicit def anyArbitrary: Arbitrary[Any] =
    Arbitrary(arbitrary[Int])
}

trait FunctionGenerators {
  def alwaysFalse[A]: NamedLambda[A, Boolean] =
    ((_: Any) => false).name

  def alwaysTrue[A]: NamedLambda[A, Boolean] =
    ((_: Any) => true).name

  def toStringFunc[A]: NamedLambda[A, String] =
    ((x: Any) => x.toString).name("toString")

  def hashCodeFunc[A]: NamedLambda[A, Int] =
    ((x: Any) => x.hashCode()).name("hashCode")

  val isEven: NamedLambda[Int, Boolean] =
    ((x: Int) => x % 2 == 0).name

  def hashCodeIsEven[A]: NamedLambda[A, Boolean] =
    hashCodeFunc andThen isEven

  def predicateGen[A]: Gen[NamedLambda[A, Boolean]] =
    Gen.oneOf(alwaysTrue, alwaysFalse, hashCodeIsEven)
}

/**
  * Instances of `Cogen[A]` allow us to generate functions that take `A` as argument.
  */
trait CogenInstances {
  implicit val keyCogen: Cogen[Key] =
    Cogen[String].contramap(_.key)

  implicit def rowUpdateCogen[K: Cogen, A: Cogen]: Cogen[RowUpdate[K, A]] =
    Cogen[Either[K, (K,A)]].contramap(_.toEither)

  implicit def iorCogen[A: Cogen, B: Cogen]: Cogen[A Ior B] =
    Cogen[Either[A, Either[B, (A,B)]]].contramap {
      case Ior.Left(a) => Left(a)
      case Ior.Right(b) => Right(Left(b))
      case Ior.Both(a,b) => Right(Right((a,b)))
    }

  implicit def anyCogen: Cogen[Any] = Cogen((seed, t) => t match {
    case x: Int => seed.reseed(x)
    case (x, y) => Cogen[(Any,Any)].perturb(seed, (x,y))
    case (x, y, z) => Cogen[(Any,Any, Any)].perturb(seed, (x,y,z))
    case x => seed.reseed(x.hashCode())
  })
}

trait ShrinkInstances {
  import restrictions.stream.{ast => stream}
  import restrictions.relation.{ast => relation}

  implicit def streamDSLShrink: Shrink[StreamDSL[Any]] =
    Shrink {
      case stream.Never() => Stream()
      case Now(_) => Stream(never)
      case FromSeq(seq) => Shrink.shrink[immutable.Seq[Any]](seq).map(FromSeq(_))
      case stream.Map(xs, _) => streamDSLShrink.shrink(xs)
      case stream.Filter(xs, _) => streamDSLShrink.shrink(xs)
      case Merge(xs, ys) => streamDSLShrink.shrink(xs) ++ streamDSLShrink.shrink(ys)
      case other =>
        streamDSLTupleShrink.shrink(other.asInstanceOf[StreamDSL[(Any, Any)]])
          .map(_.asInstanceOf[StreamDSL[Any]])
    }

  implicit def streamDSLTupleShrink: Shrink[StreamDSL[(Any, Any)]] =
    Shrink {
      case stream.Zip(xs, ys) =>
        (streamDSLShrink.shrink(xs) ++ streamDSLShrink.shrink(ys))
          .map(_.asInstanceOf[StreamDSL[(Any, Any)]])
      case _ => Stream()
    }

  implicit def relationDSLShrink[K, A]: Shrink[RelationDSL[K, A]] = Shrink {
    case ToRelation(FromSeq(seq)) => Shrink.shrink(seq).map(s => ToRelation(FromSeq(s)))
    case relation.Map(rel, f) => relationDSLShrink.shrink(rel).map(relation.Map(_, f))
    case relation.Filter(rel, predicate) => Stream(rel) ++ relationDSLShrink.shrink(rel)
    case InnerJoin(left, right) => for {
      l <- relationDSLShrink.shrink(left)
      r <- relationDSLShrink.shrink(right)
    } yield InnerJoin(l, r)
    case OuterJoin(left, right) => for {
      l <- relationDSLShrink.shrink(left)
      r <- relationDSLShrink.shrink(right)
    } yield OuterJoin(l, r)
    case other => Stream()
  }
}
