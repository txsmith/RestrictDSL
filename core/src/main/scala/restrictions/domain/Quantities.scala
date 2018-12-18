package restrictions.domain

import restrictions.domain.external._
import cats.Eq
import squants.motion.AngularVelocity
import squants.{AbstractQuantityNumeric, Quantity}

import scala.annotation.implicitNotFound
import scala.concurrent.duration.Duration
import scala.language.implicitConversions


/**
 * Instances of this type class prove that dividing Numerator by Denominator gives us a Result.
 * (modulo division by zero)
 * E.g.: Mass / Volume = Density
 */
@implicitNotFound("${Numerator} cannot be divided by ${Denominator}")
trait SafeDivide[Numerator, Denominator, Result] {
  def divide(left: Numerator, right: Denominator): Result
}
object SafeDivide {
  def apply[Numerator, Denominator, Result](implicit s: SafeDivide[Numerator, Denominator, Result]): SafeDivide[Numerator, Denominator, Result] =
    implicitly[SafeDivide[Numerator, Denominator, Result]]
}

/**
 * Instances of this prove that multiplying Left and Right results in a Result value.
 * E.g.: Velocity * Time = Length
 */
@implicitNotFound("${Left} cannot be multiplied by ${Right}")
trait SafeMultiply[Left, Right, Result] {
  def multiply(left: Left, right: Right): Result
}

object SafeMultiply {
  def apply[Left, Right, Result](implicit s: SafeMultiply[Left, Right, Result]): SafeMultiply[Left, Right, Result] =
    implicitly[SafeMultiply[Left, Right, Result]]
}

/**
 * Instances that prove we can always safely:
 * - divide two quantity of the same type to obtain a scalar value
 * - divide a quantity by a scalar
 * - multiply a quantity by a scalar
 * - multiply two scalars
 * - dividing two scalars
 */
trait SelfDivideMultiplyInstances {
  implicit def selfDivideDouble[Q <: Quantity[Q]]: SafeDivide[Q, Q, Double] = _ / _
  implicit def scaleDivideDouble[Q <: Quantity[Q]]: SafeDivide[Q, Double, Q] = _ / _
  implicit def scaleMultiplyDouble[Q <: Quantity[Q]]: SafeMultiply[Q, Double, Q] = _ * _
  implicit def scaleMultiplyDouble2[Q <: Quantity[Q]]: SafeMultiply[Double, Q, Q] = _ * _

  implicit def scaleDivideInt[Q <: Quantity[Q]]: SafeDivide[Q, Int, Q] = _ / _
  implicit def scaleMultiplyInt[Q <: Quantity[Q]]: SafeMultiply[Q, Int, Q] = _ * _

  implicit val multDouble: SafeMultiply[Double, Double, Double] = _ * _
  implicit val divDouble: SafeDivide[Double, Double, Double] = _ / _

  implicit val multInt: SafeMultiply[Int, Int, Int] = _ * _
  implicit val divInt: SafeDivide[Int, Int, Double] = _.toDouble / _

  implicit val multIntDouble: SafeMultiply[Int, Double, Double] = _ * _
  implicit val divIntDouble: SafeDivide[Int, Double, Double] = _ / _

  implicit val multDoubleInt: SafeMultiply[Double, Int, Double] = _ * _
  implicit val divDoubleInt: SafeDivide[Double, Int, Double] = _ / _
}

/**
  * Instances for dividing and multiplying quantities
  */
trait QuantityInstances extends SelfDivideMultiplyInstances {
  /**
    * Dividing and multiplying between length, time and velocity.
    */
  implicit val lengthTimeVelocityIntegral: SafeDivide[Length, Time, Velocity] = _ / _
  implicit val lengthVelocityTimeIntegral: SafeDivide[Length, Velocity, Time] = _ / _
  implicit val velocityTimeAccelerationIntegral: SafeDivide[Velocity, Time, Acceleration] = _ / _
  implicit val velocityAccelerationIntegral: SafeDivide[Velocity, Acceleration, Time] = _ / _
  implicit val lengthTimeVelocityDerivative: SafeMultiply[Velocity, Time, Length] = _ * _
  implicit val lengthTimeVelocityDerivative2: SafeMultiply[Time, Velocity, Length] = _ * _
  implicit val timeAccelerationVelocityDerivative: SafeMultiply[Time, Acceleration, Velocity] = _ * _
  implicit val timeAccelerationVelocityDerivative2: SafeMultiply[Acceleration, Time, Velocity] = _ * _

  /**
    * Dividing and multiplying between mass, volume and density.
    */
  implicit val massVolumeDensityIntegral: SafeDivide[Mass, Volume, Density] = _ / _
  implicit val massDensityVolumeIntegral: SafeDivide[Mass, Density, Volume] = _ / _
  implicit val densityVolumeMassDerivative: SafeMultiply[Density, Volume, Mass] = _ * _
  implicit val densityVolumeMassDerivative2: SafeMultiply[Volume, Density, Mass] = _ * _


  /**
   * Dividing and multiplying between Angle and Time
   */
  implicit val angleTimeIntegral: SafeDivide[Angle, Time, AngularVelocity] = _ / _
  implicit val angularVelocityTimeDerivative: SafeMultiply[AngularVelocity, Time, Angle] = _ * _

  implicit def eqQuantity[A <: Quantity[A]]: Eq[A] = (x: A, y: A) => x == y
}
