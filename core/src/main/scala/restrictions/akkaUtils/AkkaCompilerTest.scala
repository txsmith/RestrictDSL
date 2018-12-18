package restrictions.akkaUtils

import akka.actor.ActorSystem
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, OverflowStrategy}
import restrictions.compilers.StreamAkkaCompiler._
import restrictions.domain._
import restrictions.domain.external._
import restrictions.relation.ast.{RowUpdate, Update}
import restrictions.stream.ast._

object AkkaCompilerExample extends App {
  implicit val system = ActorSystem("QuickStart")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec = system.dispatcher


  val (velocityQueue, velocitySource) =
    Source.queue[RowUpdate[String, Velocity]](5, OverflowStrategy.backpressure)
      .preMaterialize()

  val velocity =
    fromAkka(velocitySource)
    .toRelation
    .filter(_ <= 10.mps)

  val velocity2 =
    fromAkka(velocitySource)
    .filter{
      case Update(_, v) => v <= 10.mps
      case _ => true
    }
    .toRelation

  compile(velocity2).toTable.onComplete { r =>
    r.get.foreach(println)
    system.terminate()
  }

  velocityQueue.offer(Update("A", 10.mps))
  velocityQueue.offer(Update("B", 5.mps))
  velocityQueue.offer(Update("A", 11.mps))
  velocityQueue.complete()

}
