package restrictions.domain

package object external extends Quantities {
  type MovementType = pronto.domain.MovementType
  val Inbound = pronto.domain.MovementType.Inbound
  val Outbound = pronto.domain.MovementType.Outbound
  val Shifting = pronto.domain.MovementType.Shifting
  val Transit = pronto.domain.MovementType.Transit

  val A11: VesselType = LiquefiedGasTanker
  val A12: VesselType = ChemicalTanker
  val A13: VesselType = OilTanker
}
