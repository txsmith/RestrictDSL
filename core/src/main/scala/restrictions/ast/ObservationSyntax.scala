package restrictions.ast

import java.time.ZonedDateTime

import _root_.pronto.domain.{Berth, EventPortcallId, MMSI, URN}
import cats.implicits._
import restrictions.SourceInfo
import restrictions.domain._
import restrictions.domain.external._

import scala.language.implicitConversions
import scala.math.Ordering

/**
 * This is what gives us the nice surface syntax. This trait is inherited by the [[restrictions]] package so importing
 * that package automatically gives you this syntax.
 */
trait ObservationSyntax {
  import OperatorSyntax._

  val wind: Observation[Wind] =
    GetWind()

  val tide: Observation[Tide] =
    GetTide()

  val tidalStream: Observation[TidalStream] =
    GetTidalStream()

  val vessel: Observation[Vessel] =
    GetVessel()

  val berth: Observation[Berth] =
    GetBerth()

  implicit def constant[A](value: A): Observation[A] =
    Constant(value)

  def not(observation: Observation[Boolean]): Observation[Boolean] =
    Not(observation)


  implicit def observationOps[A](left: Observation[A]): ObservationOps[A] =
    new ObservationOps[A](left)

  implicit def divideAndMultiplyOps[A](left: Observation[A]): DivideAndMultiplyOps[A] =
    new DivideAndMultiplyOps[A](left)

  implicit def constantDivideAndMultiplyOps[Q](value: Q): DivideAndMultiplyOps[Q] =
    divideAndMultiplyOps(constant(value))

  implicit def orderingOps[A: Ordering](left: Observation[A]): OrderingOps[A] =
    new OrderingOps[A](left)

  implicit def numericOps[A: Numeric](left: Observation[A]): NumericOps[A] =
    new NumericOps[A](left)

  implicit def booleanOps(left: Observation[Boolean]): BooleanOps =
    new BooleanOps(left)

  implicit def vesselOps(left: Observation[Vessel]): VesselOps =
    new VesselOps(left)

}
object OperatorSyntax {
  class ObservationOps[A](val left: Observation[A]) extends AnyVal {
    def ===(right: Observation[A]): Observation[Boolean] =
      is(right)
    def is(right: Observation[A]): Observation[Boolean] =
      Equals[A](left, right)
  }

  class DivideAndMultiplyOps[Q](val left: Observation[Q]) extends AnyVal {
    def /[R,S](right: Observation[R])(implicit instance: SafeDivide[Q,R,S]): Observation[S] =
      Divide(left, right, instance)
    def *[R,S](right: Observation[R])(implicit instance: SafeMultiply[Q,R,S]): Observation[S] =
      Times(left, right, instance)
  }

  class OrderingOps[A : Ordering](val left: Observation[A])(implicit sourceInfo: SourceInfo) {
    def compare(right: Observation[A]): Observation[Int] = Compare(left, right, implicitly[Ordering[A]])
    def <(right: Observation[A]): Observation[Boolean] = LessThan(left, right, implicitly[Ordering[A]])
    def >(right: Observation[A]): Observation[Boolean] = GreaterThan(left, right, implicitly[Ordering[A]])
    def <=(right: Observation[A]): Observation[Boolean] = LessOrEq(left, right, implicitly[Ordering[A]])
    def >=(right: Observation[A]): Observation[Boolean] = GreaterOrEq(left, right, implicitly[Ordering[A]])
  }

  class NumericOps[A : Numeric](val left: Observation[A]) {
    def +(right: Observation[A]): Observation[A] = Plus(left, right, implicitly[Numeric[A]])
    def -(right: Observation[A]): Observation[A] = Minus(left, right, implicitly[Numeric[A]])
    def unary_-(): Observation[A] = Negate(left, implicitly[Numeric[A]])
    def absolute(): Observation[A] = Absolute(left, implicitly[Numeric[A]])
    def signum(): Observation[Int] = Signum(left, implicitly[Numeric[A]])
  }

  class BooleanOps(val left: Observation[Boolean]) extends AnyVal {
    def and(right: Observation[Boolean]): Observation[Boolean] = left && right
    def or(right: Observation[Boolean]): Observation[Boolean] = left || right
    def &&(right: Observation[Boolean]): Observation[Boolean] = And(left, right)
    def ||(right: Observation[Boolean]): Observation[Boolean] = Or(left, right)
  }

  class VesselOps(val vesselObservation: Observation[Vessel]) {
    val getVesselMMSI = ((_:Vessel).mmsi).name("mmsi")
    val getVesselName = ((_:Vessel).name).name("name")
    val getVesselType = ((_:Vessel).vesselType).name("type")
    val getVesselDirection = ((_:Vessel).direction).name("direction")
    val getVesselDraught = ((_:Vessel).draught).name("draught")
    val getVesselBeam = ((_:Vessel).beam).name("beam")
    val getVesselLength = ((_: Vessel).length).name("length")
    val getVesselDestination = ((_:Vessel).destination).name("destination")
    val getPortcallId = restrictions.name((_:Vessel).portcallId).name("portcallId")
    val getVesselTime = restrictions.name((_:Vessel).time).name("time")

    val mmsi: Observation[MMSI] = vesselObservation.map(getVesselMMSI)
    val name: Observation[String] = vesselObservation.map(getVesselName)
    val vesselType: Observation[VesselType] = vesselObservation.map(getVesselType)
    val direction: Observation[MovementType] = vesselObservation.map(getVesselDirection)
    val draught: Observation[Length] = vesselObservation.map(getVesselDraught)
    val beam: Observation[Length] = vesselObservation.map(getVesselBeam)
    val length: Observation[Length] = vesselObservation.map(getVesselLength)
    val destination: Observation[URN] = vesselObservation.map(getVesselDestination)
    val portcallId: Observation[EventPortcallId] = vesselObservation.map(getPortcallId)
    val time: Observation[ZonedDateTime] = vesselObservation.map(getVesselTime)
  }
}

