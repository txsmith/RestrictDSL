package restrictions.domain.external

import squants.AbstractQuantityNumeric

import scala.concurrent.duration.Duration
import scala.language.implicitConversions

/**
 * Re-exports of some Squants types, primarily to avoid having to import Squants
 * when you're already importing this package.
 */
trait Quantities {
  type Time = squants.Time
  type Mass = squants.Mass
  type Volume = squants.Volume
  type Length = squants.Length
  type Velocity = squants.Velocity
  type Density = squants.Density
  type Angle = squants.Angle
  type Acceleration = squants.Acceleration

  implicit object TimeNumeric extends AbstractQuantityNumeric[Time](squants.time.Time.primaryUnit)
  implicit object MassNumeric extends AbstractQuantityNumeric[Mass](squants.mass.Mass.primaryUnit)
  implicit object VolumeNumeric extends AbstractQuantityNumeric[Volume](squants.space.Volume.primaryUnit)
  implicit object LengthNumeric extends AbstractQuantityNumeric[Length](squants.space.Length.primaryUnit)
  implicit object VelocityNumeric extends AbstractQuantityNumeric[Velocity](squants.motion.Velocity.primaryUnit)
  implicit object DensityNumeric extends AbstractQuantityNumeric[Density](squants.mass.Density.primaryUnit)
  implicit object AngleNumeric extends AbstractQuantityNumeric[Angle](squants.space.Angle.primaryUnit)

  implicit def timeToScalaDuration(time: Time): Duration = squants.time.TimeConversions.timeToScalaDuration(time)

  implicit class TimeConversions[A](n: A)(implicit num: Numeric[A]) extends squants.time.TimeConversions.TimeConversions[A](n)
  implicit class VolumeConversions[A](n: A)(implicit num: Numeric[A]) extends squants.space.VolumeConversions.VolumeConversions[A](n)
  implicit class LengthConversions[A](n: A)(implicit num: Numeric[A]) extends squants.space.LengthConversions.LengthConversions[A](n)
  implicit class MassConversions[A](n: A)(implicit num: Numeric[A]) extends squants.mass.MassConversions.MassConversions[A](n)
  implicit class DensityConversions[A](n: A)(implicit num: Numeric[A]) extends squants.mass.DensityConversions.AreaDensityConversions[A](n)
  implicit class VelocityConversions[A](n: A)(implicit num: Numeric[A]) extends squants.motion.VelocityConversions.VelocityConversions[A](n)
  implicit class AngleConversions[A](n: A)(implicit num: Numeric[A]) extends squants.space.AngleConversions.AngleConversions[A](n)

  implicit class PercentageConversion(n: Double) {
    def percent: Double = n/100
  }
}
