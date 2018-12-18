package restrictions.domain.external

import java.util

import org.apache.kafka.common.serialization.Serializer
import pronto.domain.{EventPortcallId, MMSI, URN}

import io.circe._
import io.circe.generic.semiauto._

object Encoders {
  implicit val encodeEventPortcallId: Encoder[EventPortcallId] = portcallId => Json.fromString(portcallId.id)
  implicit val encodeMMSI: Encoder[MMSI] = mmsi => Json.fromString(mmsi.id)
  implicit val encodeURN: Encoder[URN] = urn => Json.fromString(urn.encoded)
  implicit val encodeVesselType: Encoder[VesselType] = vType => Json.fromString(vType.prefix)
  implicit val encodeMovementType: Encoder[MovementType] = movement => Json.fromString(movement.toString)
  implicit val encodeLength: Encoder[Length] = length => Json.fromString(length.toString)
  implicit val encodeVessel: Encoder[Vessel] = deriveEncoder[Vessel]
  implicit val encodeRestrictEvent: Encoder[RestrictEvent] = deriveEncoder[RestrictEvent]

  implicit def encodeRowUpdate[K,A](implicit encodeKey: Encoder[K], encodeVal: Encoder[A]): Encoder[RowUpdate[K,A]] = {
    case Update(key, value) => Json.obj(
      ("type", Json.fromString("update")),
      ("key", encodeKey(key)),
      ("value", encodeVal(value))
    )
    case Delete(key) => Json.obj(
      ("type", Json.fromString("delete")),
      ("key", encodeKey(key))
    )
  }

  implicit def encoderToSerializer[A](implicit encoder: Encoder[A]): Serializer[A] = new Serializer[A] {
    override def serialize(topic: String, data: A): Array[Byte] =
      encoder(data).noSpaces.getBytes("UTF-8")

    override def configure(configs: util.Map[String, _], isKey: Boolean): Unit = {}
    override def close(): Unit = {}
  }
}
