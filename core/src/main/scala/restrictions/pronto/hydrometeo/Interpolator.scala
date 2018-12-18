package restrictions.pronto.hydrometeo

import restrictions.domain._
import restrictions.domain.external._
import java.time.{ZoneId, ZonedDateTime}
import java.time.temporal.ChronoUnit

import cats._
import cats.implicits._
import restrictions.pronto.apis.WeatherTideAPI.Measurement

/**
 * An interpolator that takes a series of discrete timed values,
 * and 'interpolates' them over time.
 * This allows us to make predictions of measurements even when the queried time
 * is between two predictions from the hydrometeo api.
 *
 * As MVP, this does linear interpolation between the two values nearest in time.
 */
class Interpolator[A](private val dataPoints: List[Timed[A]]) {

  def predict[Delta](time: ZonedDateTime)(implicit
    safeDivide: SafeDivide[A,Time,Delta],
    safeMultiply: SafeMultiply[Delta, Time, A], num: Numeric[A]): Option[A] = {
    import num.{getClass => _, _}
    import safeDivide.divide
    import safeMultiply.multiply
    /**
      * Since we're doing linear interpolation between two points,
      * we have to find a function of the shape:
      *   f(time) = a*time+b
      *
      * We can find the constants `a` and `b` of the desired function:
      * a = slope between the two known points,
      * a = deltaValue / deltaTime
      * where
      *   deltaValue = value at point B - value at point A
      *   deltaTime = time difference between A and B
      *
      * b = Av - (deltaValue / deltaTime) * At
      *
      * Putting it all together and refactoring a bit we get:
      * predict(time) = (deltaValue / deltaTime) * (time - At) + Av
      */
    def interpolate: Option[A] = for {
      (before, after) <- findNearestDataPoints(time)
      beforeTime = before.time.toInstant.getEpochSecond.seconds
      queryTime = time.toInstant.getEpochSecond.seconds
      afterTime = after.time.toInstant.getEpochSecond.seconds
      deltaV = after.value - before.value
      deltaT = afterTime - beforeTime
    } yield multiply(divide(deltaV, deltaT), queryTime - beforeTime) + before.value

    /**
      * In the case that there is only one data point available, we can
      * make a prediction if the queried time is relatively close in
      * time to that data point by just taking that point as prediction
      */
    def pickOne: Option[A] = for {
      point <- dataPoints.sortBy(x => Math.abs(ChronoUnit.MINUTES.between(x.time, time))).headOption
      if Math.abs(point.time.until(time, ChronoUnit.MINUTES)) <= 60
    } yield point.value

    interpolate orElse pickOne
  }

  /**
    * Find the pair of data points that surround the given time.
    * That is, both points are next to each other in the sequence
    * of data points and one is timestamped before the query time
    * while the other is timed after the query time.
    */
  private def findNearestDataPoints(queryTime: ZonedDateTime): Option[(Timed[A], Timed[A])] =
    dataPoints zip dataPoints.tail find {
      case (early, late) => early.time.isBefore(queryTime) && late.time.isAfter(queryTime)
    }


  /**
    * Discard irrelevant data points.
    * Given a time, this returns a new interpolator where all data
    * points before the given time are removed.
    */
  def removeBefore(trimTime: ZonedDateTime): Interpolator[A] =
    new Interpolator(dataPoints filter { _.time isAfter trimTime })
}

object Interpolator {
  def empty[A] = new Interpolator[A](List.empty)

  def apply[A](value: Timed[A]) =
    new Interpolator(List(value))

  def apply[A](measurement: Measurement) =
    new Interpolator(List(Timed(measurement.observationTime, measurement)))

  def apply[A](measurement: List[Measurement]) =
    new Interpolator(measurement map { m => Timed(m.observationTime, m)})

  implicit def interpolationMonoidK: MonoidK[Interpolator] = new MonoidK[Interpolator] {
    implicit val zonedDateTimeOrdering: Ordering[ZonedDateTime] = _ compareTo _

    override def empty[A]: Interpolator[A] = Interpolator.empty[A]

    override def combineK[A](x: Interpolator[A], y: Interpolator[A]) =
      new Interpolator[A]((x.dataPoints ++ y.dataPoints).sortBy(_.time))
  }

  implicit val timedFunctor: Functor[Timed] = new Functor[Timed] {
    override def map[A, B](fa: Timed[A])(f: A => B) = Timed(fa.time, f(fa.value))
  }

  implicit val interpolatorFunctor: Functor[Interpolator] = new Functor[Interpolator] {
    override def map[A, B](fa: Interpolator[A])(f: A => B) = new Interpolator(fa.dataPoints.map(_.map(f)))
  }

  implicit def interpolationMonoid[A]: Monoid[Interpolator[A]] = interpolationMonoidK.algebra

  implicit def interpolatorEq[A: Eq]: Eq[Interpolator[A]] = (x: Interpolator[A], y: Interpolator[A]) =>
    x.dataPoints === y.dataPoints

  implicit def timedEq[A: Eq]: Eq[Timed[A]] = (x: Timed[A], y: Timed[A]) =>
    x.time == y.time && x.value === y.value
}

case class Timed[A](time: ZonedDateTime, value: A)
object InterpolateTest extends App {
  val now = ZonedDateTime.now()
  val now2 = ZonedDateTime.now(ZoneId.of("UTC"))
  println(now.toInstant.getEpochSecond)
  println(now2.toInstant.getEpochSecond)
  val future = now.plusHours(10)
  val inbetween = now.plusHours(8)

  val pointA = Timed(now, 10.meters)
  val pointB = Timed(future, 100.meters)
  val interpolated = (Interpolator(pointA) |+| Interpolator(pointB)).predict(inbetween)
  println(interpolated)
}