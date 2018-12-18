package restrictions.domain

import pronto.domain.{BerthId, GeneralBerthId, PortmapsBerthId}

import scala.language.implicitConversions

/**
  * Identifiers can be specific numbers,
  * or a wildcard indicating any number will match.
  */
sealed trait WildcardIdentifier
case class NumericIdentifier(n: Long) extends WildcardIdentifier {
  override def toString: String = n.toString
}
case object AnyIdentifier extends WildcardIdentifier {
  override def toString: String = "any"
}

/**
  * SAP Identifiers.
  * Can be denoted by using N_Z100/{section}/{location}
  * For example: `Z100/6/any` is a valid SAP Identifier.
  *
  * @param countryCode the country of the location
  * @param sectionCode the high-level section of the location
  * @param locationCode the specific identifier of the location within its section
  */
final case class BerthIdentifier(countryCode: NumericIdentifier,
                                 sectionCode: NumericIdentifier,
                                 locationCode: WildcardIdentifier) {
  override def toString: String = "Z" + countryCode + "/" + sectionCode + "/" + locationCode
}

object BerthIdentifier {
  def apply(protoBerthID: BerthId): Option[BerthIdentifier] = protoBerthID match {
    case GeneralBerthId(_) => Option.empty
    case PortmapsBerthId(id) => fromString(id)
  }

  def fromString(id: String): Option[BerthIdentifier] = {
    val ZCode = ".*Z100\\/(\\d+)\\/(\\d+)".r

    id match {
      case ZCode(x,y) => Option(Z100/x.toInt/y.toInt)
      case _ => Option.empty
    }
  }
}

object Z100 {
  def /(section: NumericIdentifier): IncompleteSAPIdentifier = IncompleteSAPIdentifier(100, section)
}

final case class IncompleteSAPIdentifier(countryCode: NumericIdentifier,
                                         sectionCode: NumericIdentifier) {
  def /(locationCode: WildcardIdentifier): BerthIdentifier =
    BerthIdentifier(countryCode, sectionCode, locationCode)
}