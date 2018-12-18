package restrictions.pronto.kafka

import java.time.ZonedDateTime

import akka.stream.ActorMaterializer
import play.api.Logger
import pronto.domain.EventPortcallId
import pronto.kafka.KafkaTopic
import pronto.kafka.KafkaJson._
import pronto.domain.json.VisitJsonProtocol._
import pronto.domain.ProntoVisit
import restrictions._
import restrictions.domain.external.Vessel
import restrictions.relation.ast.{RelationDSL, RowUpdate}
import restrictions.stream.ast._

import scala.concurrent.ExecutionContext

object VisitsTableKafka {
  def getVesselTable(implicit ec: ExecutionContext, mat: ActorMaterializer, logger: Logger): RelationDSL[EventPortcallId, Vessel] = {
    val vessels = KafkaConsumer.startKafkaConsumer[ProntoVisit](KafkaTopic.Visit) via VisitToVessel.toVessel

    val vesselUpdates = vessels map { vessel => RowUpdate(vessel.portcallId, vessel) }

    val activePortcall: NamedLambda[Vessel, Boolean] =
      ((vessel: Vessel) => {
        val now = ZonedDateTime.now()
        if (now.isAfter(vessel.time)) {
          logger.warn(s"🗑 Ignoring visit because port call is in the past: ${vessel.portcallId.id}")
          false
        } else {
          logger.warn(s"Succesfully ingested ${vessel.name}: ${vessel.portcallId.id}")
          true
        }
      }).name

    fromAkka(vesselUpdates).toRelation filter activePortcall
  }
}
