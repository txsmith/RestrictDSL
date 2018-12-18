package restrictions.pronto.kafka

import akka.{Done, NotUsed}
import akka.kafka.ConsumerMessage
import akka.stream.scaladsl.Source
import akka.stream.{ActorMaterializer, OverflowStrategy, QueueOfferResult}
import org.apache.kafka.clients.consumer.OffsetResetStrategy
import org.apache.kafka.common.serialization._
import org.apache.kafka.common.utils.Bytes
import play.api.Logger
import pronto.kafka.KafkaConnectionService.AutoOffsetResetConfig
import pronto.kafka.KafkaTopic

import scala.concurrent.{ExecutionContext, Future}

object KafkaConsumer {

  implicit val bytesDeserializer: Deserializer[Bytes] = new BytesDeserializer

  def startKafkaConsumer[A: Deserializer](topic: KafkaTopic, resetStrategy: AutoOffsetResetConfig = AutoOffsetResetConfig.Latest)(
    implicit
      materializer: ActorMaterializer,
      executionContext: ExecutionContext,
      logger: Logger
  ): Source[A, NotUsed] = {

    val (queue, events: Source[A, NotUsed]) =
      Source.queue[A](10000, OverflowStrategy.backpressure).preMaterialize()

    val kafka = KafkaConnectionProvider.getKafkaConnectionService
    kafka.consumeTopicAndDo[Bytes, A](
      topic = topic,
      restart_on_failure = false,
      reset_strategy = resetStrategy) {
      case (_, Some(v)) => queue.offer(v).flatMap {
        case QueueOfferResult.QueueClosed => Future.failed(new RuntimeException("Queue closed"))
        case x => Future.successful(x)
      }
      case _ => Future {}
    }
    logger.warn(s"Starting Kafka consumer: ${topic.entryName}")
    events.log("Error")
  }

  def startKafkaConsumerWithoutCommit[A: Deserializer](topic: KafkaTopic, resetStrategy: AutoOffsetResetConfig = AutoOffsetResetConfig.Latest)(
    implicit
      materializer: ActorMaterializer,
    executionContext: ExecutionContext,
    logger: Logger
  ): Source[Option[A], NotUsed] = {

    val kafka = KafkaConnectionProvider.getKafkaConnectionService
    logger.warn(s"Starting Kafka consumer (not committing): ${topic.entryName}")
    kafka.consumeTopic[Bytes, A](
      topic = topic,
      reset_strategy = resetStrategy
    ).flatMapMerge(Int.MaxValue, {
      case (_, partitionSource) =>
        partitionSource.map(msg => Option(msg.record.value()))
    }).mapMaterializedValue(_ => NotUsed).log("Error")
  }

  def skipEvents(topic: KafkaTopic)(
      implicit
      materializer: ActorMaterializer,
      executionContext: ExecutionContext,
      logger: Logger
      ): Source[Unit, NotUsed] = {


    val (queue, events: Source[Unit, NotUsed]) =
      Source.queue[Unit](100, OverflowStrategy.backpressure).preMaterialize()

    val kafka = KafkaConnectionProvider.getKafkaConnectionService

    kafka.consumeTopicAndDo[Bytes, Bytes](
      topic = topic,
      restart_on_failure = false,
      reset_strategy = AutoOffsetResetConfig.Latest
    ) {
      case _ => queue.offer({}).flatMap {
        case QueueOfferResult.QueueClosed => Future.failed(new RuntimeException("Queue closed"))
        case x => Future.successful(x)
      }
    }
    events
  }
 }
