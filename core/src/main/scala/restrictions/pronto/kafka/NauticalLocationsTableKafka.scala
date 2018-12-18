package restrictions.pronto.kafka

import akka.stream.ActorMaterializer
import play.api.Logger
import play.api.libs.json.JsResult
import pronto.kafka.KafkaTopic
import pronto.domain.{Berth, Location, NauticalLocation, URN}
import pronto.kafka.KafkaJson._
import pronto.domain.json.NauticalLocationJsonProtocol._
import pronto.kafka.KafkaConnectionService.AutoOffsetResetConfig.Earliest
import restrictions.relation.ast.{RelationDSL, RowUpdate}
import restrictions.stream.ast.fromAkka

import scala.concurrent.ExecutionContext

object NauticalLocationsTableKafka {
  def getNauticalLocationsTable(implicit ec: ExecutionContext, mat: ActorMaterializer, logger: Logger): RelationDSL[URN, Berth] = {
    val nauticalLocations = KafkaConsumer.startKafkaConsumerWithoutCommit[JsResult[NauticalLocation]](
      KafkaTopic.NauticalLocationMasterData, Earliest
    ) filter
      { _.isDefined } map { _.get } filter
      { _.isSuccess } map { _.get } filter
      { _.`type` == Location.Berth} map { _.asInstanceOf[Berth] } map
      { l => RowUpdate(l.urn, l) }

    fromAkka(nauticalLocations).toRelation
  }
}
