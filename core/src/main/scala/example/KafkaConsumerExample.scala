package example

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.Logger
import play.api.libs.json.{JsResult, JsSuccess}
import pronto.domain.NauticalLocation
import pronto.kafka.KafkaConnectionService.AutoOffsetResetConfig.Earliest
import pronto.kafka.KafkaTopic
import restrictions.pronto.kafka.{KafkaConnectionProvider, KafkaConsumer}
import pronto.kafka.KafkaJson._
import pronto.domain.json.NauticalLocationJsonProtocol._

object KafkaConsumerExample extends App {

  implicit val actorSys = ActorSystem("KafkaConsumerExample")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec = actorSys.dispatcher
  implicit val logger: Logger = Logger(getClass)

  // TODO: don't rely on resetOffset
  KafkaConnectionProvider.resetOffset
  KafkaConsumer.startKafkaConsumerWithoutCommit[JsResult[NauticalLocation]](
    KafkaTopic.NauticalLocationMasterData, Earliest
  ).runForeach {
    case Some(JsSuccess(loc, _)) => logger.warn(loc.name)
    case _ => //
  }
}
