package restrictions.pronto.apis

import java.time.format.DateTimeFormatter
import java.time.{ZoneId, ZonedDateTime}
import java.util.UUID

import cats.implicits._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import play.api.Logger
import pronto.domain.Geometry
import pronto.domain.Point
import pronto.domain.JtsInterop._
import restrictions.domain._
import restrictions.domain.external._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class WeatherTideAPI(apiKey: String) {
  import WeatherTideAPI._

  private val logger = Logger(getClass)
  private val isoDateTimeFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  def fetch(sensorTypes: SensorType*)(implicit ec: ExecutionContext): Future[Either[String, Vector[Measurement]]] = {
    sensorTypes.toList foldMap fetchRaw map { apiCallResult =>
      for {
        allObservations <- apiCallResult
        allMeasurements <- allObservations traverse JsonStructure.observationToMeasurement
      } yield allMeasurements.flatten
    }
  }

  def fetchRaw(sensorType: SensorType)(implicit ec: ExecutionContext): Future[Either[String, Vector[JsonStructure.Observation]]] = {
    val fromTime = ZonedDateTime.now(ZoneId.of("UTC")).minusMinutes(60)
    val untilTime = fromTime.plusHours(24)
    logger.warn(s"Fetching WeatherTideAPI: ${sensorType}")
    Future {
      val response = requests.get("https://api.portofrotterdam.com/v1/weather-tide/observations",
        headers = Map("apikey" -> apiKey),
        params = Map(
          "from-time" -> fromTime.format(isoDateTimeFormat),
          "to-time" -> untilTime.format(isoDateTimeFormat),
          "parametertype" -> sensorType.apiString
        ),
        verifySslCerts = false,
        readTimeout = 60000
      )
      logger.warn(s"Received response for ${sensorType}")
      if (response.is2xx) {
        JsonStructure.parseResponseText(response.text)
      } else {
        Left(s"Error fetching weather-tide API (${response.statusCode}): ${response.text}")
      }
    }
  }

}
object WeatherTideAPI {
  import JsonStructure.MeasurementReference
  import MeasurementTypes.Multiple

  abstract class SensorType(private[WeatherTideAPI] val apiString: String)
  case object TideSensor extends SensorType("HTIDE")
  case object WindSensor extends SensorType("WIND")
  case object TidalStreamSensor extends SensorType("TDSTR")
  case object PressureSensor extends SensorType("PRES")
  case object TemperatureSensor extends SensorType("TEMP")
  case object SalinitySensor extends SensorType("SALI")
  case object WaveSensor extends SensorType("WAVE")
  case object HumiditySensor extends SensorType("HUM")
  case object VisibilitySensor extends SensorType("VISIB")

  /**
    * Convenient representation of the measurements returned by the API.
    * When given a value of type `Measurement`, you can pattern match on it as follows:
    *
    * ```
    * measurement match {
    *   case wind.velocity(speed, location) => ... // speed: Velocity, location: HydroMeteoSensorLocation
    *   case MeasurementCodes.PWD10(direction, location) => ... // direction: Angle, location: HydroMeteoSensorLocation
    * }
    * ```
    */
  sealed trait Measurement {
    val observationTime: ZonedDateTime
    val measurementCode: String
    val location: HydroMeteoSensorLocation
  }

  object wind {
    object predicted {
      val direction = MeasurementCodes.PWD10
      val velocity = MeasurementCodes.PWV10
    }
    object actual {
      val direction = MeasurementCodes.WD10
      val velocity = MeasurementCodes.WV10
    }
    val direction = Multiple(predicted.direction, actual.direction)
    val velocity = Multiple(predicted.velocity, actual.velocity)
  }
  object tide {
    object predicted {
      val highWater = MeasurementCodes.PHW
      val lowWater = MeasurementCodes.PLW
      val height = MeasurementCodes.PH10

      object stream {
        val rate = Multiple(MeasurementCodes.PTSR10, MeasurementCodes.PTSRDA10)
        val direction = Multiple(MeasurementCodes.PTSD10, MeasurementCodes.PTSDDA10)
      }
    }
    object actual {
      val highWater = MeasurementCodes.HW
      val lowWater = MeasurementCodes.LW
      val height = MeasurementCodes.H10

      object stream {
        val rate = Multiple(MeasurementCodes.TSR10, MeasurementCodes.TSRBA10)
        val direction = MeasurementCodes.TSD10
      }
    }

    val highWater = Multiple(predicted.highWater, actual.highWater)
    val lowWater = Multiple(predicted.lowWater, actual.lowWater)
    val height = Multiple(predicted.height, actual.height)
    object stream {
      val rate = Multiple(predicted.stream.rate, actual.stream.rate)
      val direction = Multiple(predicted.stream.direction, actual.stream.direction)
    }
  }

  object MeasurementCodes {
    import MeasurementTypes._
    val PWD10 = AngleMeasurementType("PWD10", "Predicted wind direction 10 minutes", _.degrees)
    val PWV10 = VelocityMeasurementType("PWV10", "Predicted wind velocity 10 minutes", _.mps)
    val WD10 = AngleMeasurementType("WD10", "Wind direction 10 minutes", _.degrees)
    val WV10 = VelocityMeasurementType("WV10", "Wind velocity 10 minutes", _.mps)

    val PHW = LengthMeasurementType("PHW", "Predicted high water", _.centimeters)
    val PLW = LengthMeasurementType("PLW", "Predicted low water", _.centimeters)
    val PH10 = LengthMeasurementType("PH10", "Predicted height of tide 10 minutes", _.centimeters)
    val HW = LengthMeasurementType("HW", "High water", _.centimeters)
    val LW = LengthMeasurementType("LW", "Low water", _.centimeters)
    val H10 = LengthMeasurementType("H10", "Height of tide 10 minutes", _.centimeters)

    val TSR01 = VelocityMeasurementType("TSR01", "Tidal stream rate 1 minute", _.mps)
    val TSR10 = VelocityMeasurementType("TSR10", "Tidal stream rate 10 minutes", _.mps)
    val TSRBA10 = VelocityMeasurementType("TSRBA10", "Tidal stream rate box averaged 10 minutes", _.mps)
    val PTSR10 = VelocityMeasurementType("PTSR10", "Predicted tidal stream rate 10 minutes", _.mps)
    val PTSR10S15 = VelocityMeasurementType("PTSR10S15", "Predicted tidal stream rate 10 minutes from surface to 15 meters", _.mps)
    val PTSR10S5 = VelocityMeasurementType("PTSR10S5", "Predicted tidal stream rate 10 minutes from surface to 5 meters", _.mps)
    val PTSRDA10 = VelocityMeasurementType("PTSRDA10", "Predicted tidal stream rate 10 minutes depth averaged", _.mps)
    val PMXS = VelocityMeasurementType("PMXS", "Predicted tidal stream rate maximum", _.mps)
    val PMXSDA = VelocityMeasurementType("PMXSDA", "Predicted tidal stream rate maximum depth averaged", _.mps)
    val PMXSS15 = VelocityMeasurementType("PMXSS15", "Predicted tidal stream rate maximum from surface to 15 meters", _.mps)
    val PMXSS5 = VelocityMeasurementType("PMXSS5", "Predicted tidal stream rate maximum from surface to 5 meters", _.mps)

    val TSD01 = AngleMeasurementType("TSD01", "Tidal stream direction 1 minute", _.degrees)
    val TSD10 = AngleMeasurementType("TSD10", "Tidal stream direction 10 minutes", _.degrees)
    val PTSD10 = AngleMeasurementType("PTSD10", "Predicted tidal stream direction 10 minutes", _.degrees)
    val PTSD10S15 = AngleMeasurementType("PTSD10S15", "Predicted tidal stream direction 10 minutes from surface to 15 meters", _.degrees)
    val PTSD10S5 = AngleMeasurementType("PTSD10S5", "Predicted tidal stream direction 10 minutes from surface to 5 meters", _.degrees)
    val PTSDDA10 = AngleMeasurementType("PTSDDA10", "Predicted tidal stream direction 10 minutes depth averaged", _.degrees)

    val SLW = VelocityMeasurementType("SLW", "Sack water", _.mps)
  }

  case class VelocityMeasurement(measurementCode: String,
                                 observationTime: ZonedDateTime,
                                 location: HydroMeteoSensorLocation,
                                 value: Velocity,
                                 reference: Option[MeasurementReference]) extends Measurement

  case class LengthMeasurement(measurementCode: String,
                               observationTime: ZonedDateTime,
                               location: HydroMeteoSensorLocation,
                               value: Length,
                               reference: Option[MeasurementReference]) extends Measurement

  case class DensityMeasurement(measurementCode: String,
                                observationTime: ZonedDateTime,
                                location: HydroMeteoSensorLocation,
                                value: Density,
                                reference: Option[MeasurementReference]) extends Measurement

  case class AngleMeasurement(measurementCode: String,
                              observationTime: ZonedDateTime,
                              location: HydroMeteoSensorLocation,
                              value: Angle,
                              reference: Option[MeasurementReference]) extends Measurement

  object MeasurementTypes {
    case class Multiple[A](measurementTypes: MeasurementType[A]*) extends MeasurementType[A] {
      override def unapply(measurement: Measurement): Option[(A, HydroMeteoSensorLocation)] =
        measurementTypes.foldRight(Option.empty[(A,HydroMeteoSensorLocation)]) {
          (measurementType, next) => measurementType.unapply(measurement).orElse(next)
        }
    }


    trait MeasurementType[A] {
      def unapply(arg: Measurement): Option[(A, HydroMeteoSensorLocation)]
    }
    case class AngleMeasurementType(measurementCode: String, description: String, convert: Double => Angle) extends MeasurementType[Angle] {
      def apply(time: ZonedDateTime, location: HydroMeteoSensorLocation, value: Double, reference: Option[MeasurementReference]): Measurement =
        AngleMeasurement(measurementCode, time, location, convert(value), reference)

      override def unapply(arg: Measurement): Option[(Angle, HydroMeteoSensorLocation)] = arg match {
        case AngleMeasurement(code, _, location, value, _) if code == measurementCode => Option((value, location))
        case _ => Option.empty
      }
    }

    case class VelocityMeasurementType(measurementCode: String, description: String, convert: Double => Velocity) extends MeasurementType[Velocity] {
      def apply(time: ZonedDateTime, location: HydroMeteoSensorLocation, value: Double, reference: Option[MeasurementReference]): Measurement =
        VelocityMeasurement(measurementCode, time, location, convert(value), reference)

      def unapply(arg: Measurement): Option[(Velocity, HydroMeteoSensorLocation)] = arg match {
        case VelocityMeasurement(code, _, location, value, _) if code == measurementCode => Option((value, location))
        case _ => Option.empty
      }
    }

    case class LengthMeasurementType(measurementCode: String, description: String, convert: Double => Length) extends MeasurementType[Length] {
      def apply(time: ZonedDateTime, location: HydroMeteoSensorLocation, value: Double, reference: Option[MeasurementReference]): Measurement =
        LengthMeasurement(measurementCode, time, location, convert(value), reference)

      def unapply(arg: Measurement): Option[(Length, HydroMeteoSensorLocation)] = arg match {
        case LengthMeasurement(code, _, location, value, _) if code == measurementCode => Option((value, location))
        case _ => Option.empty
      }
    }

    case class DensityMeasurementType(measurementCode: String, description: String, convert: Double => Density) extends MeasurementType[Density] {
      def apply(time: ZonedDateTime, location: HydroMeteoSensorLocation, value: Double, reference: Option[MeasurementReference]): Measurement =
        DensityMeasurement(measurementCode, time, location, convert(value), reference)

      def unapply(arg: Measurement): Option[(Density, HydroMeteoSensorLocation)] = arg match {
        case DensityMeasurement(code, _, location, value, _) if code == measurementCode => Option((value, location))
        case _ => Option.empty
      }
    }
  }

  /**
    * Types and parsers for the JSON returned by the API.
    * The API's JSON structure is left fully intact in these types.
    * Use if the types above do not suit your needs.
    */
  object JsonStructure {
    import MeasurementCodes._

    def observationToMeasurement(observation: Observation): Either[String, Vector[Measurement]] = {
      val params = observation.result.flatMap(r => r.parameters map { p => (p.name, r.phenomenonTime, p.value) }).sortBy(_._1)
      val paramsInfo = observation.procedure.parameters.groupBy(_.name).mapValues(_.head)
      featureToLocation(observation.featureOfInterest) map { location =>
        params flatMap {
          case (PWV10.measurementCode, time, value) => Vector(PWV10(time, location, value, paramsInfo(PWV10.measurementCode).reference))
          case (PWD10.measurementCode, time, value) => Vector(PWD10(time, location, value, paramsInfo(PWD10.measurementCode).reference))
          case (WD10.measurementCode, time, value) => Vector(WD10(time, location, value, paramsInfo(WD10.measurementCode).reference))
          case (WV10.measurementCode, time, value) => Vector(WV10(time, location, value, paramsInfo(WV10.measurementCode).reference))

          case (PHW.measurementCode, time, value) => Vector(PHW(time, location, value, paramsInfo(PHW.measurementCode).reference))
          case (PLW.measurementCode, time, value) => Vector(PLW(time, location, value, paramsInfo(PLW.measurementCode).reference))
          case (PH10.measurementCode, time, value) => Vector(PH10(time, location, value, paramsInfo(PH10.measurementCode).reference))
          case (HW.measurementCode, time, value) => Vector(HW(time, location, value, paramsInfo(HW.measurementCode).reference))
          case (LW.measurementCode, time, value) => Vector(LW(time, location, value, paramsInfo(LW.measurementCode).reference))
          case (H10.measurementCode, time, value) => Vector(H10(time, location, value, paramsInfo(H10.measurementCode).reference))

          case (PTSR10.measurementCode, time, value) => Vector(PTSR10(time, location, value, paramsInfo(PTSR10.measurementCode).reference))
          case (PTSD10.measurementCode, time, value) => Vector(PTSD10(time, location, value, paramsInfo(PTSD10.measurementCode).reference))
          case (PTSRDA10.measurementCode, time, value) => Vector(PTSRDA10(time, location, value, paramsInfo(PTSRDA10.measurementCode).reference))
          case (PTSDDA10.measurementCode, time, value) => Vector(PTSDDA10(time, location, value, paramsInfo(PTSDDA10.measurementCode).reference))
          case (TSR10.measurementCode, time, value) => Vector(TSR10(time, location, value, paramsInfo(TSR10.measurementCode).reference))
          case (TSRBA10.measurementCode, time, value) => Vector(TSRBA10(time, location, value, paramsInfo(TSRBA10.measurementCode).reference))
          case (TSD10.measurementCode, time, value) => Vector(TSD10(time, location, value, paramsInfo(TSD10.measurementCode).reference))
          case _ => Vector()
        }
      }
    }

    def featureToLocation(feature: Feature): Either[String, HydroMeteoSensorLocation] =
      featureToGeometry(feature) map { HydroMeteoSensorLocation(feature.location, UUID.fromString(feature.id), _) }

    def featureToGeometry(feature: Feature): Either[String, Geometry] = feature.geometry match {
      case FeatureGeometry("Point", x +: y +: _, spatialReference) => toEPSG4326(Point(x,y), spatialReference)
      case FeatureGeometry("Point", _, _) => Left("Found a geometry with less than two coordinates")
      case FeatureGeometry(geomType, _, _) => Left(s"Found non-Point geometry: $geomType")
    }

    def toEPSG4326(geometry: Geometry, sourceSpatialReference: String): Either[String, Geometry] = {
      val prontoSpatialRef = "EPSG:4326"
      val prontoCRS = CRS.decode(prontoSpatialRef, true)
      val sourceCRS = CRS.decode(s"EPSG:$sourceSpatialReference")
      val transform = Try(CRS.findMathTransform(sourceCRS, prontoCRS)).fold(
        _ => Left(s"Could not convert EPSG:$sourceSpatialReference to $prontoSpatialRef"),
        Right(_))

      transform map {
        JTS.transform(geometry.asJtsGeom, _).asProntoGeom
      }
    }

    def parseResponseText(text: String): Either[String, Vector[JsonStructure.Observation]] = {
      parse(text)
        .valueOr(x => throw x)
        .hcursor
        .downField("_embedded")
        .get[Vector[JsonStructure.Observation]]("observations").leftMap(_.toString)
    }

    case class Observation(parameterType: ParameterType,
                           procedure: ObservationProcedure,
                           featureOfInterest: Feature,
                           result: Vector[ObservationResult])

    case class ObservationProcedure(name: String,
                                    description: String,
                                    parameters: Vector[Parameter])

    case class Parameter(name: String,
                         description: String,
                         unitOfMeasure: UnitOfMeasure,
                         reference: Option[MeasurementReference])

    sealed trait MeasurementReference
    case object TrueNorth extends MeasurementReference
    case object AmsterdamOrdinanceDatum extends MeasurementReference // NAP

    object MeasurementReference {
      def fromString(refString: String): Option[MeasurementReference] = refString match {
        case "NAP" => Option(AmsterdamOrdinanceDatum)
        case "Ntrue" => Option(TrueNorth)
        case _ => Option.empty
      }
    }

    case class ParameterType(name: String, description: String)

    case class UnitOfMeasure(name: String)

    case class Feature(id: String, location: String, geometry: FeatureGeometry)

    case class FeatureGeometry(geomType: String, coordinates: Vector[Double], spatialReference: String)

    case class ObservationResult(phenomenonTime: ZonedDateTime, parameters: Vector[ParameterValue])

    case class ParameterValue(name: String, value: Double)

    implicit val decodeParamType: Decoder[ParameterType] = (c: HCursor) => for {
      n <- c.get[String]("type")
      d <- c.get[String]("description")
    } yield ParameterType(n, d)

    implicit val decodeParam: Decoder[Parameter] = (c: HCursor) => for {
      n <- c.get[String]("name")
      d <- c.get[String]("description")
      u <- c.get[String]("uom")
      r <- c.get[Option[String]]("reference")
    } yield Parameter(n, d, UnitOfMeasure(u), r.map(MeasurementReference.fromString(_).get))

    implicit val decodeGeometry: Decoder[FeatureGeometry] = (c: HCursor) => for {
      t <- c.get[String]("type")
      coords <- c.get[Vector[Double]]("coordinates")
      ref <- c.downField("spatialReference").get[String]("wkid")
    } yield FeatureGeometry(t, coords, ref)
  }
}
