package restrictions
package stream
package interpreter

import akka.NotUsed
import akka.stream.scaladsl._
import org.scalacheck.Prop
import org.scalacheck.Prop.exists
import org.scalatest.{FlatSpec, Matchers, PropSpec}
import org.scalatest.prop.Checkers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import restrictions.relation.ast.{RelationDSL, RowUpdate}
import restrictions.stream.ast._
import restrictions.akkaUtils._
import restrictions.compilers.StreamAkkaCompiler

import scala.collection.immutable


class AkkaStreamInterpreterProperties extends
  PropSpec with
  GeneratorDrivenPropertyChecks with
  AkkaDSLHelpers with
  Matchers {
  import GenStreamDSL._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 500, workers = 8, sizeRange = 20)

  def compile[Key,A](relationDSL: RelationDSL[Key,A]): UpdateSource[Key, A] = StreamAkkaCompiler.compile(relationDSL)
  def compile[Key,A](streamDSL: StreamDSL[A]): Source[A, NotUsed] = StreamAkkaCompiler.compile(streamDSL)

  /**
    * All StreamDSL terms should terminate when interpreted and evaluated
    */
  property("interpreting and evaluation terminates") {
    forAll { stream: StreamDSL[Int] => eval(stream) }
  }

  property("evaluation always has a unique outcome") {
    forAll { stream: StreamDSL[Int] =>
      stream evaluatesTo stream
    }
  }

  /**
    * Test the identity and composition properties.
    * Ideally we'd like to test these with *any* StreamDSL term as input,
    * i.e.: for any StreamDSL, the following should hold:
    *   eval(streamDSL map id) == eval(streamDSL)
    *
    * However, due to the non-deterministic nature of akka streams these
    * properties do not hold. So here we only test them for 'simple' StreamDSL
    * terms that are created directly from an akka source.
    */

  property("map identity") {
    forAll { source: Source[Int, NotUsed] =>
      val stream = fromAkka(source)
      (stream map id) emitsSameAs stream
    }
  }


  property("map composition") {
    forAll { (source: Source[Int, NotUsed], f: Int => String, g: String => Double) =>
      val stream = fromAkka(source)
      (stream map (f andThen g)) evaluatesTo (stream map f map g)
    }
  }

  property("filter identity") {
    forAll { source: Source[Int, NotUsed] =>
      val stream = fromAkka(source)
      stream filter (_ => true) evaluatesTo stream
    }
  }

  property("filter composition") {
    forAll { (source: Source[Int, NotUsed], f: Int => Boolean, g: Int => Boolean) =>
      val stream = fromAkka(source)
      (stream filter f filter g) evaluatesTo (stream filter (x => f(x) && g(x)))
    }
  }

  property("merge right identity") {
    forAll { source: Source[Int, NotUsed] =>
      val stream = fromAkka(source)
      stream merge never evaluatesTo stream
    }
  }

  property("merge left identity") {
    forAll { source: Source[Int, NotUsed] =>
      val stream = fromAkka(source)
      never merge stream evaluatesTo stream
    }
  }

  property("merge is associative") {
    forAll { (as: Source[Int, NotUsed], bs: Source[Int, NotUsed], cs: Source[Int, NotUsed]) =>
      val xs = fromAkka(as)
      val ys = fromAkka(bs)
      val zs = fromAkka(cs)
      ((xs merge ys) merge zs) emitsSameAs (xs merge (ys merge zs))
    }
  }


  case object Q

  property("zip right identity") {
    forAll { source: Source[Int, NotUsed] =>
      val stream = fromAkka(source)
      stream zip now(Q) emitsSubsetOf (stream map ((_, Q)))
    }
  }

  property("zip left identity") {
    forAll { source: Source[Int, NotUsed] =>
      val stream = fromAkka(source)
      now(Q) zip stream emitsSubsetOf (stream map ((Q, _)))
    }
  }

  /**
    * These tests all check commutativity of some operation:
    *
    *   interpret(op(streamDSL)) == op(interpret(streamDSL))
    *
    * That is, if some operation (called `op`) is defined on StreamDSL,
    * then performing that operation on any streamDSL term should be
    * equivalent to performing that operation after interpreting streamDSL.
    *
    * Note that these properties hold trivially since this is how the
    * interpreter is implemented.
    */

  property("map commutes under evaluation") {
    forAll { (stream: StreamDSL[Int], f: Int => Int) =>
      (stream map f) evaluatesTo (compile(stream) map f)
    }
  }

  property("filter commutes under evaluation") {
    forAll { (stream: StreamDSL[Int], f: Int => Boolean) =>
      (stream filter f) evaluatesTo (compile(stream) filter f)
    }
  }

  property("merge commutes under evaluation") {
    forAll { (left: StreamDSL[Int], right: StreamDSL[Int]) =>
      (left merge right) evaluatesTo (compile(left) merge compile(right))
    }
  }

  property("zip commutes under evaluation") {
    forAll { (left: StreamDSL[Int], right: StreamDSL[Int]) =>
      (left zip right) evaluatesTo (compile(left) amazingZipLatest compile(right))
    }
  }

  property("stream.toRelation.changelog == stream") {
    forAll { stream: StreamDSL[RowUpdate[Key, Int]] =>
      stream.toRelation.changelog evaluatesTo stream
    }
  }
}

/**
  * These tests are canaries for properties that we wish we had, but don't.
  * My intuition for these properties is that they do not hold because of inconsistent
  * behavior in Akka, which can hopefully be fixed.
  *
  * These tests will fail as soon as the properties DO hold, signaling that we should
  * change the tests above.
  */
class AkkaStreamInterpreterCanaries extends
  FlatSpec with
  GeneratorDrivenPropertyChecks with
  AkkaDSLHelpers with
  Matchers {
  import GenStreamDSL._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 500, workers = 8, sizeRange = 20)

  "Map identity" should "not change the stream" in {
    check(exists { stream: StreamDSL[Int] =>
      eval(stream map id) != eval(stream)
    })
  }
  def reassociate[A,B,C](t: ((A, B), C)): (A, (B, C)) = (t._1._1, (t._1._2, t._2))

  // We can't test zip for associativity
  // Because the output depends completely on the timing of events.
  // We'd be able to test this if we have a way to control for timing of events.
  "zip on streams" should "be associative" in {
    check(exists { t: (immutable.Seq[Int], immutable.Seq[Int], immutable.Seq[Int]) =>
      (t._1.nonEmpty && t._2.nonEmpty && t._3.nonEmpty: Prop) ==> {
        val xs = fromSeq(t._1)
        val ys = fromSeq(t._2)
        val zs = fromSeq(t._3)

        val left = (xs zip ys) zip zs
        val right = xs zip (ys zip zs)
        eval(left map reassociate) != eval(right)
      }
    })
  }
}
