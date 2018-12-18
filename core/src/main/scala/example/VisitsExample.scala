package example

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.Logger
import pronto.domain.{EventPortcallId, URN}
import restrictions.akkaUtils.TableViewerSink
import restrictions.compilers.StreamAkkaCompiler
import restrictions.pronto.kafka.VisitsTableKafka
import restrictions.relation.ast.RelationDSL

/**
  * Show all vessels in a dynamically changing table
  */
object VisitsExample extends App {
  implicit val actorSys = ActorSystem("VisitExample")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec = actorSys.dispatcher
  implicit val logger: Logger = Logger(getClass)

  val vessels: RelationDSL[EventPortcallId, (String, URN)] = VisitsTableKafka.getVesselTable.map { vessel =>
    logger.warn(s"New vessel signal: ${vessel.name}")
    (vessel.name, vessel.destination)
  }
  val akkaSource = StreamAkkaCompiler.compile(vessels)
  akkaSource.runWith(new TableViewerSink[EventPortcallId, (String, URN)](Seq("EventPortcallId", "Name", "URN")))
}
