package restrictions.execution

import akka.Done
import akka.stream.scaladsl._
import pronto.domain.EventPortcallId
import restrictions._
import restrictions.domain.external._
import restrictions.compilers.{RestrictionCompiler, StreamAkkaCompiler}
import restrictions.domain.external.RestrictEvent
import restrictions.pronto.hydrometeo.HydroMeteoTables
import pronto.kafka.{NauticalLocationsTableKafka, RestrictEventProducer, VisitsTableKafka}
import restrictions.pronto.hydrometeo.HydroMeteoTables.HydroMeteo
import restrictions.relation.ast.{Delete, RelationDSL, RowUpdate, Update}
import restrictions.relation.ast.RelationOptimizer._

import scala.concurrent.Future

object ExecuteRestrictions extends App {
  import RestrictionActorSystem._

  def execAndPublish(rules: Restrict): Future[Done] =
    execute(rules, RestrictEventProducer.getRestrictionsSink)

  def execute[A](rules: Restrict, sink: Sink[RowUpdate[EventPortcallId, RestrictEvent], A]): A = {
    val vesselTable: RelationDSL[EventPortcallId, Vessel] = VisitsTableKafka.getVesselTable
    val HydroMeteo(wind, tide, tidalStream, hydroMeteoRefreshTimer) = HydroMeteoTables.getHydroMeteoTables
    val locationsTable = NauticalLocationsTableKafka.getNauticalLocationsTable

    val compiler = RestrictionCompiler(wind, tide, tidalStream, vesselTable, locationsTable)
    val resultTable: RelationDSL[EventPortcallId, Result] =
      compiler.compile(rules)

    val restrictEvents =
      (vesselTable join resultTable).changelog.map {
        case Update(portcallId, (vessel, Deny(reason))) => RowUpdate(portcallId, RestrictEvent(vessel, reason))
        case Update(portcallId, _) => RowUpdate.delete(portcallId)
        case Delete(portcallId) => RowUpdate.delete(portcallId)
      }.toRelation

    val result = StreamAkkaCompiler.compile(optimise(restrictEvents)).runWith(sink)
    hydroMeteoRefreshTimer.cancel()
    result
  }
}
