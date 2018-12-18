package restrictions.domain

import scala.language.implicitConversions

trait LocationDefinitions {
  implicit def locationSet(locs: Set[Location]): Location = LocationSet(locs)
  implicit def berthLocation(identifier: BerthIdentifier): BerthLocation = BerthLocation(identifier)

  val any: AnyIdentifier.type = AnyIdentifier

  implicit def anyIdToLocation(any: AnyIdentifier.type): Location = AnyLoc
  implicit def intToNumericID(n: Int): NumericIdentifier = NumericIdentifier(n)
  implicit def longToNumericID(n: Long): NumericIdentifier = NumericIdentifier(n)
}
