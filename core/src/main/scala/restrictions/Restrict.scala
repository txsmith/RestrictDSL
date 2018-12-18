package restrictions

import _root_.pronto.domain.Berth
import cats.implicits._
import restrictions.ast._
import restrictions.domain._
import restrictions.domain.external._

abstract class Restrict(val location: Location) {

  case class Environment(windObservation: Observation[Wind],
                         tideObservation: Observation[Tide],
                         tidalStreamObservation: Observation[TidalStream],
                         berthObservation: Observation[Berth]) {

    val wind: WindF[Observation] = WindF[Observation](
      velocity = windObservation.map(((_: Wind).velocity).name("velocity")),
      direction = windObservation.map(((_: Wind).direction).name("direction")),
    )

    val tide: TideF[Observation] = TideF[Observation](
      tideObservation.map(((_: Tide).height).name("height")),
      tideObservation.map(((_: Tide).high_water).name("high_water")),
      tideObservation.map(((_: Tide).low_water).name("low_water"))
    )

    val tidalStream: TidalStreamF[Observation] = TidalStreamF[Observation](
      rate = tidalStreamObservation.map(((_: TidalStream).rate).name("rate")),
      direction = tidalStreamObservation.map(((_: TidalStream).direction).name("direction")),
    )

    val berth: BerthF[Observation] = BerthF(
      berthObservation.map(((_: Berth).length.meters).name("length")),
      berthObservation.map(((_: Berth).width.meters).name("width")),
      berthObservation.map(((_: Berth).draught.meters).name("draught")),
    )
  }

  protected val environment = Environment(wind, tide, tidalStream, berth)

  private[restrictions] def getStatements: RestrictStatement = RestrictLocation(location, statements)
  private var statements: List[RestrictStatement] = List.empty

  protected def restrict(location: Location)(restrictions: => Unit)(implicit s: SourceInfo): Unit = {
    val previous = statements
    statements = List.empty
    restrictions
    statements = previous :+ RestrictLocation(location, statements)
  }

  protected def require(condition: Observation[Boolean])(implicit s: SourceInfo): Unit = {
    statements = statements :+ Require(condition)
  }

  protected def when(condition: Observation[Boolean])(restrictions: => Unit)(implicit s: SourceInfo): Unit = {
    val previous = statements
    statements = List.empty
    restrictions
    statements = previous :+ When(condition, statements)
  }

  def prettyPrint: String = PrettyPrinter.pretty(getStatements)

  def sourceInfo: SourceInfo = statements.head.sourceInfo
}

