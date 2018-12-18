package restrictions.domain.external

sealed abstract class VesselType(val prefix: String)
case object Cargo extends VesselType("A")
case object Tanker extends VesselType("A1")
case object LiquefiedGasTanker extends VesselType("A11")
case object ChemicalTanker extends VesselType("A12")
case object OilTanker extends VesselType("A13")
case object OtherLiquidTanker extends VesselType("A14")
case object BulkCarrier extends VesselType("A2")
case object DryBulk extends VesselType("A21")
case object DryBulkAndOil extends VesselType("A22")
case object DryBulkSelfDischarging extends VesselType("A23")
case object OtherDryBulk extends VesselType("A24")
case object GeneralCargo extends VesselType("A31")
case object PassengerAndGeneralCargo extends VesselType("A32")
case object Container extends VesselType("A33")
case object RefrigeratedCargo extends VesselType("A34")
case object RoRoCargo extends VesselType("A35")
case object PassengerAndRoRoCargo extends VesselType("A36")
case object Passenger extends VesselType("A37")
case object OtherDryCargo extends VesselType("A38")
case object Working extends VesselType("B")
case object TowingPushing extends VesselType("B32")
case object PilotVessel extends VesselType("B34N")
case class Other(code: String) extends VesselType(code)

object VesselType {
  val allTypes = List(
    Cargo, Tanker, LiquefiedGasTanker, ChemicalTanker,
    OilTanker, OtherLiquidTanker, BulkCarrier, DryBulk,
    DryBulkAndOil, DryBulkSelfDischarging, OtherDryBulk, GeneralCargo,
    PassengerAndGeneralCargo, Container, RefrigeratedCargo, RoRoCargo,
    PassengerAndRoRoCargo, Passenger, OtherDryCargo, Working,
    TowingPushing, PilotVessel)

  def apply(statCode: String): VesselType =
    allTypes.reverse.find(t => statCode.startsWith(t.prefix)).getOrElse(Other(statCode))
}
