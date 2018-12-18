package restrictions.compilers

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.Logger
import pronto.domain.{EventPortcallId, Berth, URN}
import restrictions.akkaUtils.UpdateSource
import restrictions.Restrict
import restrictions.domain._
import restrictions.domain.external._
import restrictions.pronto.hydrometeo.Interpolator
import restrictions.relation.ast.{RelationDSL, RelationOptimizer}

import scala.concurrent.ExecutionContext

case class RestrictionAkkaCompiler(wind: RelationDSL[UUID, WindF[Interpolator]],
                                   tide: RelationDSL[UUID, TideF[Interpolator]],
                                   vesselTable: RelationDSL[EventPortcallId, Vessel],
                                   berthTable: RelationDSL[URN, Berth]) {
  def compile(rules: Restrict)
             (implicit sys: ActorSystem, mat: ActorMaterializer, ec: ExecutionContext, logger: Logger): UpdateSource[EventPortcallId, Result] = {

    // TODO implement
    val restrictCompiler = RestrictionCompiler(wind, tide, ???, vesselTable, berthTable)
    val compiledToTable = restrictCompiler.compile(rules)
    val optimized = RelationOptimizer.optimise(compiledToTable)
    StreamAkkaCompiler.compile(optimized)
  }
}
