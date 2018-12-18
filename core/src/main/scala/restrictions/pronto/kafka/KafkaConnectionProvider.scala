package restrictions.pronto.kafka

import java.math.BigInteger
import java.security.SecureRandom
import java.util.{Collections, Properties}

import com.heroku.sdk.EnvKeyStore
import org.apache.kafka.clients.consumer.{ConsumerRecords, KafkaConsumer, OffsetAndMetadata}
import org.apache.kafka.common.TopicPartition
import play.api.Configuration
import pronto.kafka.KafkaConnectionService
import pronto.kafka.config._

import scala.collection.JavaConverters._
import scala.concurrent.duration._

object KafkaConnectionProvider {
  def readResource(fileName: String): String = {
    scala.io.Source.fromResource(fileName).mkString
  }

  val sslConf: SSLConfig = SSLConfig(
    trustedCert = readResource("trusted.crt"),
    clientCert = readResource("client-2.crt"),
    clientCertKey = readResource("client-2.key")
  )

  def getKafkaConnectionService: KafkaConnectionService = {
    val batchConf: BatchConfig = BatchConfig.default
    val consumerConf: AkkaKafkaConsumerConfig = AkkaKafkaConsumerConfig.default.copy(`kafka-clients` =
      Configuration(
        "enable.auto.commit" -> false,
        "group.id" -> "test.thomas",
        "bootstrap.servers" -> "kafka+ssl://pronto-kafka-pronto.aivencloud.com:18506"))
    val producerConf: AkkaKafkaProducerConfig = AkkaKafkaProducerConfig.default.copy(`kafka-clients` =
      Configuration("bootstrap.servers" -> "kafka+ssl://pronto-kafka-pronto.aivencloud.com:18506"))

    val conf: KafkaConfig = KafkaConfig(
      prefix = "base.",
      batch = batchConf,
      backoff = Some(BackoffConfig(1.seconds, 10.seconds, 0.2)),
      ssl = Some(sslConf),
      akkaKafkaConsumer = consumerConf,
      akkaKafkaProducer = producerConf
    )

    new KafkaConnectionService(conf)
  }


  /**
    * Ugly hack.
    * Sometimes I wish to reset the kafka offset for the nautical locations topic to the beginning.
    * This is extremely awkward to do through the `pronto.kafka` and alpakka libraries,
    * so here I'm just calling the Kafka API directly myself.
    */
  def resetOffset: Unit = {

    val password = new BigInteger(130, new SecureRandom).toString(32)

    val envTrustStore: EnvKeyStore =
      EnvKeyStore.createFromPEMStrings(sslConf.trustedCert, password)

    val envKeyStore: EnvKeyStore =
      EnvKeyStore.createFromPEMStrings(sslConf.clientCertKey, sslConf.clientCert, password)

    val trustStore = envTrustStore.storeTemp()
    val keyStore = envKeyStore.storeTemp()

    val props = new Properties()
    props.put("bootstrap.servers", "kafka+ssl://pronto-kafka-pronto.aivencloud.com:18506")
    props.put("group.id", "test.thomas")
    props.put("enable.auto.commit", "false")
    props.put("auto.commit.interval.ms", "1000")
    props.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
    props.put("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer")
    props.put("security.protocol", "SSL")
    props.put("ssl.truststore.type", envKeyStore.`type`)
    props.put("ssl.truststore.location", trustStore.getAbsolutePath)
    props.put("ssl.truststore.password", envTrustStore.password)
    props.put("ssl.keystore.type", envKeyStore.`type`)
    props.put("ssl.keystore.location", keyStore.getAbsolutePath)
    props.put("ssl.keystore.password", envKeyStore.password)

    val consumer = new KafkaConsumer[String, String](props)
    consumer.partitionsFor("base.master-nautical-location").asScala.foreach(info => {
      val partition = new TopicPartition(info.topic(), info.partition())
      consumer.assign(Collections.singletonList(partition))
      val r: ConsumerRecords[String, String] = consumer.poll(java.time.Duration.ofSeconds(1))
      consumer.seek(partition, 23243)
      consumer.commitSync(Collections.singletonMap(partition, new OffsetAndMetadata(23243)))
    })

  }
}
