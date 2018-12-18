package restrictions.pronto.apis

import java.time.{Instant, ZoneId, ZonedDateTime}

import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import play.api.Logger
import pronto.domain.MMSI
import squants._
import squants.motion.AngularVelocity
import squants.motion.AngularVelocityConversions._
import squants.motion.VelocityConversions._
import squants.space.AngleConversions._
import squants.space.LengthConversions._
import squants.space.{Angle, Length}

case class AisApi(username: String, authKey: String) {
  import AisApi._

  private val baseURL: String = "https://pronto-ais-api-base.herokuapp.com/v0/"
  private val logger: Logger = Logger(getClass)

  def vesselInfo(mmsis: Seq[MMSI]): Seq[VesselInfo] = {
    val response = requests.post(
      url = baseURL + "ship/mmsilist",
      auth = (username, authKey),
      headers = List(("Content-Type", "application/json")),
      data = mmsis.map(_.id).mkString("[", ",", "]")
    )

    if (response.is2xx) {
      val responseText = response.text()
      decode[Seq[VesselInfo]](responseText).valueOr(x => {
        logger.error(s"Error decoding AIS vessel info: $responseText")
        throw x
      })
    } else {
      throw new RuntimeException(response.statusCode + " " + response.statusMessage)
    }
  }

  def vesselInfo(mmsi: MMSI): VesselInfo =
    vesselInfo(Seq(mmsi)).head
}
object AisApi {
  case class VesselInfo(
//                       eta: ZonedDateTime,
//                       imo: pronto.domain.IMO,
                       mmsi: MMSI,
//                       status: String,
                       callSign: String,
//                       location: pronto.domain.Point,
//                       shipType: String,
                       maxDraught: Length,
//                       rateOfTurn: AngularVelocity,
//                       destination: String,
//                       trueHeading: Angle,
                       timeLastUpdate: ZonedDateTime,
//                       speedOverGround: Velocity,
//                       courseOverGround: Angle,
//                       positionAccuracy: Boolean,
//                       transponderClass: String,
//                       positionSensorType: String,
//                       positionOfTransponder: PointOnShip
//                       specialManeuverIndicator: String
                       )

  case class PointOnShip(
                        distanceToBow: Length,
                        distanceToPort: Length,
                        distanceToStern: Length,
                        distanceToStarboard: Length
                        )

  implicit val decodeMMSI: Decoder[MMSI] =
    _.as[String].map(MMSI(_))

  implicit val decodeIMO: Decoder[pronto.domain.IMO] =
    _.as[String].map(pronto.domain.IMO(_))

  implicit val decodeLatLong: Decoder[pronto.domain.Point] =
    _.get[List[BigDecimal]]("coordinates").map {
      coords => pronto.domain.Point(coords(0), coords(1))
    }

  implicit val decodeUnixTimestamp: Decoder[ZonedDateTime] =
    _.as[Long].map(epoch => ZonedDateTime.ofInstant(Instant.ofEpochMilli(epoch), ZoneId.of("GMT")))

  implicit val decodeDegrees: Decoder[Angle] =
    _.as[Double].map(_.degrees)

  implicit val decodeMeters: Decoder[Length] =
    _.as[Double].map(_.meters)

  implicit val decodeKnots: Decoder[Velocity] =
    _.as[Double].map(_.knots)

  implicit val decodeDegreesPerSec: Decoder[AngularVelocity] =
    _.as[Double].map(_.degreesPerSecond)
}