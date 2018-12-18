import restrictions.domain._
import restrictions.domain.external._

import scala.util.{Failure, Success, Try}

trait Read[A] { def reads(s: String): Try[A] }

object Read {
  import RuleParser._

  def apply[A](implicit readA: Read[A]): Read[A] = readA

  implicit object stringRead extends Read[String] {
    def reads(s: String): Try[String] = Success(s)
  }

  implicit object intRead extends Read[Int] {
    def reads(s: String) = Try(s.toInt)
  }

  implicit object doubleRead extends Read[Double] {
    def reads(s: String) = Try(s.toDouble).orElse(Try(s.replace(',', '.').toDouble))
  }

  implicit object vesselTypeRead extends Read[VesselType] {
    override def reads(s: String): Try[VesselType] = s match {
      case "A11A" => Success(LiquefiedGasTanker)
      case "A12" => Success(ChemicalTanker)
      case "A13" => Success(OilTanker)
      case _ => Failure(new RuntimeException(s"Unknown vessel type: $s"))
    }
  }

  implicit object optionVesselTypeRead extends Read[Option[VesselType]] {
    override def reads(s: String): Try[Option[VesselType]] = s match {
      case "ALL" => Success(None)
      case _ => optionRead[VesselType].reads(s)
    }
  }

  implicit def optionRead[A](implicit readA: Read[A]): Read[Option[A]] = {
    case "" => Success(None)
    case s => readA.reads(s).map(Some(_))
  }

  implicit object sapIdentifierRead extends Read[BerthIdentifier] {
    override def reads(s: String): Try[BerthIdentifier] = BerthIdentifier.fromString(s).fold[Try[BerthIdentifier]](
      Failure(new RuntimeException(s"Could not parse berth id: $s"))
    )(
      Success(_)
    )
  }

  implicit object restrTypeRead extends Read[RestrictionType] {
    override def reads(s: String): Try[RestrictionType] = s match {
      case "MINIMUM_UKC" => Success(MINIMUM_UKC)
      case "MINIMUM_UKC_LONG_TERM" => Success(MINIMUM_UKC_LONG_TERM)
      case "FRESH_WATER_ALLOWANCE" => Success(FRESH_WATER_ALLOWANCE)
      case "VESSEL_TYPE" => Success(VESSEL_TYPE)
      case "MAXIMUM_BEAM" => Success(MAXIMUM_BEAM)
      case "MINIMUM_LENGTH" => Success(MINIMUM_LENGTH)
      case "MAXIMUM_LENGTH" => Success(MAXIMUM_LENGTH)
      case _ => Failure(new RuntimeException(s"Unknown restriction type: $s"))
    }
  }




}
