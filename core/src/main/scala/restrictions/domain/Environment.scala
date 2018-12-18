package restrictions.domain

import cats._
import cats.implicits._
import restrictions._
import restrictions.pronto.hydrometeo.Interpolator
import restrictions.domain.external._
import scala.language.higherKinds

trait Environment {
  type Wind = WindF[Id]
  type Tide = TideF[Id]
  type Berth = BerthF[Id]
  type TidalStream = TidalStreamF[Id]
  type InterpolatedWind = WindF[Interpolator]
  type InterpolatedTide = TideF[Interpolator]
  type InterpolatedTidalStream = TidalStreamF[Interpolator]

  def Wind(velocity: Velocity, direction: Angle): Wind =
    WindF[Id](velocity, direction)

  case class WindF[F[_]](velocity: F[Velocity],
                         direction: F[Angle])


  def Tide(height: Length, high_water: Length, low_water: Length): Tide =
    TideF[Id](height, high_water, low_water)

  case class TideF[F[_]](height: F[Length],
                         high_water: F[Length],
                         low_water: F[Length])

  def TidalStream(rate: Velocity, direction: Angle): TidalStream =
    TidalStreamF[Id](rate, direction)

  case class TidalStreamF[F[_]](rate: F[Velocity], direction: F[Angle])

  def Berth(length: Length, width: Length, draught: Length): Berth =
    BerthF[Id](length, width, draught)

  case class BerthF[F[_]](length: F[Length],
                          width: F[Length],
                          draught: F[Length])

  implicit val WindFTraverse: HTraverse[WindF] = new HTraverse[WindF] {
    override def traverse[F[_], G[_], A[_]: Applicative](f: F ~> Compose[A,G,?])(t: WindF[F]): A[WindF[G]] =
      (f(t.velocity), f(t.direction)).mapN(WindF[G] _)

    override def map[F[_], G[_]](f: F ~> G)(t: WindF[F]): WindF[G] =
      WindF[G](f(t.velocity), f(t.direction))
  }

  implicit val TideFTraverse: HTraverse[TideF] = new HTraverse[TideF] {
    override def traverse[F[_], G[_], A[_]: Applicative](f: F ~> Compose[A,G,?])(t: TideF[F]): A[TideF[G]] =
      (f(t.height), f(t.high_water), f(t.low_water)).mapN(TideF[G] _)

    override def map[F[_], G[_]](f: F ~> G)(t: TideF[F]): TideF[G] =
      TideF[G](f(t.height), f(t.high_water), f(t.low_water))
  }

  implicit val TidalStreamFTraverse: HTraverse[TidalStreamF] = new HTraverse[TidalStreamF] {
    override def traverse[F[_], G[_], A[_]: Applicative](f: F ~> Compose[A,G,?])(t: TidalStreamF[F]): A[TidalStreamF[G]] =
      (f(t.rate), f(t.direction)).mapN(TidalStreamF[G] _)

    override def map[F[_], G[_]](f: F ~> G)(t: TidalStreamF[F]): TidalStreamF[G] =
      TidalStreamF[G](f(t.rate), f(t.direction))
  }

  implicit val BertBerthaverse: HTraverse[BerthF] = new HTraverse[BerthF] {
    override def traverse[F[_], G[_], A[_]: Applicative](f: F ~> Compose[A,G,?])(t: BerthF[F]): A[BerthF[G]] =
      (f(t.length), f(t.width), f(t.draught)).mapN(BerthF[G] _)

    override def map[F[_], G[_]](f: F ~> G)(t: BerthF[F]): BerthF[G] =
      BerthF[G](f(t.length), f(t.width), f(t.draught))
  }
}
