import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime, ZonedDateTime}

import org.scalatest.{FunSpec, Matchers}
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import cats.implicits._
import squants.motion.Velocity
import squants.{Angle, Density, Length, Quantity}
import restrictions.domain._
import restrictions.domain.external._

class MeasurementsTest extends FunSpec with Matchers {


  sealed trait Measurement {
    val observationTime: ZonedDateTime
  }
  case class VelocityMeasurement(observationTime: ZonedDateTime, measurementType: MeasurementType[Velocity], value: Velocity) extends Measurement
  case class LengthMeasurement(observationTime: ZonedDateTime, measurementType: MeasurementType[Length], value: Length) extends Measurement
  case class DensityMeasurement(observationTime: ZonedDateTime, measurementType: MeasurementType[Density], value: Density) extends Measurement
  case class AngleMeasurement(observationTime: ZonedDateTime, measurementType: MeasurementType[Angle], value: Angle) extends Measurement

  case class MeasurementType[Q <: Quantity[Q]]( parameterName: String,
                                                description: String,
                                                reference: Option[ParamReference] = Option.empty)

  object wind {
    object predicted {
      val direction = MeasurementType[Angle]("PWD10", "Predicted wind direction 10 minutes", Option(TrueNorth))
      val velocity = MeasurementType[Velocity]("PWV10", "Predicted wind velocity 10 minutes")
    }
  }

  def observationToMeasurement(observation: Observation): Vector[Measurement] = {
    val rs: Vector[(String, ZonedDateTime, Double)] = observation.result.flatMap(r => r.parameters map { p => (p.name, r.phenomenonTime, p.value)})
    rs flatMap {
      case (wind.predicted.direction.parameterName, time, value) =>
        Vector(AngleMeasurement(time, wind.predicted.direction, value.degrees))
      case (wind.predicted.velocity.parameterName, time, value) =>
        Vector(VelocityMeasurement(time, wind.predicted.velocity, value.mps))
      case _ => Vector()
    }
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
                       reference: Option[ParamReference])

  sealed trait ParamReference
  case object TrueNorth extends ParamReference
  case object AmsterdamOrdinanceDatum extends ParamReference // NAP
  object ParamReference {
    def fromString(refString: String): Option[ParamReference] = refString match {
      case "NAP" => Option(AmsterdamOrdinanceDatum)
      case "Ntrue" => Option(TrueNorth)
      case _ => Option.empty
    }
  }

  case class ParameterType(name: String, description: String)

  case class UnitOfMeasure(name: String)

  case class Feature(id: String, location: String, geometry: FeatureGeometry)

  case class FeatureGeometry(geomType: String, coordinates: Vector[Double])

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
  } yield Parameter(n, d, UnitOfMeasure(u), r.map(ParamReference.fromString(_).get))

  implicit val decodeGeometry: Decoder[FeatureGeometry] = (c: HCursor) => for {
    t <- c.get[String]("type")
    c <- c.get[Vector[Double]]("coordinates")
  } yield FeatureGeometry(t, c)

  def printTable(entries: Seq[Seq[Any]], heading: String*) =
    println(Tabulator.format(heading +: entries))

  describe("The measurements JSON") {

    val LAST_DOWNLOAD = LocalDateTime.parse("2019-07-10T14:11")
    val text = scala.io.Source.fromInputStream(getClass.getResourceAsStream("measurements.json")).mkString

    val observations: Vector[Observation] =
      Vector() ++
        parse(text)
          .valueOr(x => throw x)
          .hcursor
          .downField("_embedded")
          .get[Vector[Observation]]("observations")
          .right.get


    val procedures: Vector[ObservationProcedure] = observations.map(_.procedure)

    it("has wind measurements") {
      val wind = observations.flatMap(observationToMeasurement)
      wind.foreach(println)
    }


    it("has different parameter types") {
      val parameterTypes = observations
        .map(_.parameterType)
        .distinct
        .map(t => Seq(t.name, t.description))

      printTable(parameterTypes, "Name", "Description")
    }

    it("has different concrete parameters") {
      val params = procedures
        .flatMap(_.parameters)
        .distinct
        .sortBy(_.name)
        .map(param => Seq(param.name, param.description, param.unitOfMeasure.name))

      printTable(params, "Name", "Description", "Unit of measure")
    }

    it("may have multiple parameters per procedure") {
      procedures.filter(p => p.parameters.length > 1).foreach(println)
    }

    it("has different locations") {
      val entries = observations
        .map(o => (o.featureOfInterest.location, o.result.length))
        .groupBy(_._1)
        .mapValues(_.foldRight(0)((t, acc) => t._2 + acc))
        .toVector
        .sorted
        .map(t => List(t._1, t._2))

      printTable(entries, "Location", "# of results")
    }

    it("has some observations without results") {
      observations.filter(_.result.isEmpty)
        .foreach(println)
    }

    it("has different kinds of reference points") {
      observations
        .flatMap(_.procedure.parameters)
        .map(_.reference)
        .filter(_.isDefined)
        .map(_.get)
        .distinct
        .foreach(println)
    }

    it("has predictions in for different measurements at different points in time") {
      printTable(
        observations.
          flatMap(_.result.flatMap(r => r.parameters.map { p =>
            (p.name, ChronoUnit.HOURS.between(LAST_DOWNLOAD, r.phenomenonTime))
          }))
          .filter(_._1.startsWith("P"))
          .sorted
          .distinct
          .groupBy(_._1)
          .map(t => Seq(t._1, t._2.map(_._2).mkString(", "))).toList,
        "Prediction name", "Hours into the future"
      )
    }

    it("for each procedure parameter, there is a result") {
      observations.foreach { obs =>
        val procedureParams = obs.procedure.parameters.map(_.name)
        obs.result.foreach { r =>
          r.parameters.map(_.name) should contain allElementsOf procedureParams
        }
      }
    }
  }

  // Courtesy of https://stackoverflow.com/questions/7539831/scala-draw-table-to-console
  object Tabulator {
    def format(table: Seq[Seq[Any]]) = table match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- table) yield for (cell <- row) yield if (cell == null) 0 else cell.toString.length
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- table) yield formatRow(row, colSizes)
        formatRows(rowSeparator(colSizes), rows)
    }

    def formatRows(rowSeparator: String, rows: Seq[String]): String = (
      rowSeparator ::
        rows.head ::
        rowSeparator ::
        rows.tail.toList :::
        rowSeparator ::
        List()).mkString("\n")

    def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
      val cells = for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item)
      cells.mkString("|", "|", "|")
    }

    def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString("+", "+", "+")
  }
}
