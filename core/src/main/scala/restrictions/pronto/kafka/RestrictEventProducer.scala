package restrictions.pronto.kafka

import akka.Done
import akka.stream.scaladsl.Sink
import pronto.domain.EventPortcallId
import pronto.kafka.KafkaConnectionService.ProntoKafkaMessage
import pronto.kafka.KafkaTopic
import restrictions.domain.external._
import restrictions.relation.ast.{Delete, RowUpdate, Update}

import scala.concurrent.Future

object RestrictEventProducer {
  import Encoders._

  def getRestrictionsSink: Sink[RowUpdate[EventPortcallId, RestrictEvent], Future[Done]] = {
    val kafka = KafkaConnectionProvider.getKafkaConnectionService
    val sink: Sink[ProntoKafkaMessage[EventPortcallId, RowUpdate[EventPortcallId, RestrictEvent], _], Future[Done]] =
      kafka.produceTopicSink[EventPortcallId, RowUpdate[EventPortcallId, RestrictEvent]](KafkaTopic.Restrictions)

    sink.contramap[RowUpdate[EventPortcallId, RestrictEvent]] {
      case event@Update(key, _) => kafka.createMessage(KafkaTopic.Restrictions)(key, event, ())
      case event@Delete(key) => kafka.createMessage(KafkaTopic.Restrictions)(key, event, ())
    }
  }

}

