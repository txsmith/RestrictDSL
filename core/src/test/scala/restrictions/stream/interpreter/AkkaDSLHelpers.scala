package restrictions.stream.interpreter

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.testkit.CallingThreadDispatcher
import org.scalatest.{Assertion, Matchers}
import restrictions.akkaUtils.UpdateSource
import restrictions.compilers.StreamAkkaCompiler
import restrictions.stream.ast._
import restrictions.relation.ast._
import restrictions.compilers.StreamAkkaCompiler._

import scala.collection._
import scala.concurrent.Await
import scala.concurrent.duration._

trait AkkaDSLHelpers { self: Matchers =>

  implicit val system = ActorSystem("AkkaInterpreterTest")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec = system.dispatchers.lookup(CallingThreadDispatcher.Id)

  def eval[A](s: StreamDSL[A]): Seq[A] =
    eval(StreamAkkaCompiler.compile(s))
  
  def eval[K,A](r: RelationDSL[K,A]): collection.Map[K,A] =
    Await.result(StreamAkkaCompiler.compile(r).toTable, 4.seconds)

  def eval[A](s: Source[A, NotUsed]): Seq[A] =
    Await.result(s.runWith(Sink.seq), 4.seconds)

  implicit class StreamDSLMatcherOps[A](stream: StreamDSL[A]) {
    def evaluatesTo(resultStream: Source[A, NotUsed]): Assertion =
      evaluatesTo(fromAkka(resultStream))

    def evaluatesTo(resultStream: StreamDSL[A]): Assertion =
      eval(stream) shouldEqual eval(resultStream)

    def emitsSameAs(resultStream: StreamDSL[A])(implicit ord: Ordering[A]): Assertion =
      eval(stream).sorted shouldEqual eval(resultStream).sorted

    def emitsSubsetOf(result: StreamDSL[A]): Assertion =
      eval(result) should contain allElementsOf eval(stream)
  }

  implicit class RelationDSLMatcherOps[K,A](relation: RelationDSL[K,A]) {
    def evaluatesTo(resultStream: UpdateSource[K, A]): Assertion =
      evaluatesTo(fromAkka(resultStream).toRelation)

    def evaluatesTo(resultRelation: RelationDSL[K,A]): Assertion =
      eval(relation) shouldEqual eval(resultRelation)

    def evaluatesToSubsetOf(result: RelationDSL[K,A]): Assertion =
      eval(result).toList should contain allElementsOf eval(relation)
  }

  implicit class TraversableOnceMatcherOps[A](left: TraversableOnce[A]) {
    def isSubsetOf(right: TraversableOnce[A]): Assertion =
      right.toList should contain allElementsOf left.toList
  }
}
