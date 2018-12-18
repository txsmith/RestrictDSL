package restrictions.domain.external

import java.nio.charset.StandardCharsets
import java.util

import org.apache.kafka.common.serialization.Deserializer
import pronto.domain.{EventPortcallId, MMSI, URN}

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode
import cats.implicits._

object Decoders {
  implicit val decodeEventPortcallId: Decoder[EventPortcallId] = _.as[String].map(EventPortcallId.parse)
  implicit val decodeMMSI: Decoder[MMSI] = _.as[String].map(MMSI(_))
  implicit val decodeURN: Decoder[URN] = _.as[String].map(URN.fromString)
  implicit val decodeVesselType: Decoder[VesselType] = _.as[String].map(VesselType(_))
  implicit val decodeMovementType: Decoder[MovementType] = _.as[String].map(pronto.domain.MovementType.namesToValuesMap(_))
  implicit val decodeLength: Decoder[Length] = _.as[String].map(squants.space.Length.parseString).map(_.get)
  implicit val decodeVessel: Decoder[Vessel] = deriveDecoder[Vessel]
  implicit val decodeRestrictEvent: Decoder[RestrictEvent] = deriveDecoder[RestrictEvent]

  implicit def decodeRowUpdate[K,A](implicit decodeKey: Decoder[K], decodeVal: Decoder[A]): Decoder[RowUpdate[K,A]] = {
    val parseKey: Decoder[K] = _.downField("key").as[K]
    val parseValue: Decoder[A] = _.downField("value").as[A]
    val parseUpdate = (parseKey, parseValue).mapN(RowUpdate(_,_))
    val parseDelete = parseKey.map(RowUpdate.delete[K,A])
    parseUpdate <+> parseDelete
  }

  implicit def decoderToSerializer[A](implicit decoder: Decoder[A]): Deserializer[A] = new Deserializer[A] {
    override def deserialize(topic: String, data: Array[Byte]): A = {
      decode[A](new String(data, StandardCharsets.UTF_8)) match {
        case Left(err) => throw new RuntimeException(s"Invalid JSON: ${err}")
        case Right(value) => value
      }
    }

    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
    override def close(): Unit = {}
  }
}
