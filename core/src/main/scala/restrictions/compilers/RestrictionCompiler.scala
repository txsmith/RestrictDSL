package restrictions.compilers

import java.util.UUID

import pronto.domain.{EventPortcallId, Geometry, URN, Berth}
import cats.data.NonEmptyList
import cats.{Applicative, Apply, ~>}
import cats.implicits._
import restrictions.{Observation => _, _}
import restrictions.ast._
import restrictions.domain._
import restrictions.domain.external._
import restrictions.pronto.hydrometeo.Interpolator
import restrictions.stream.ast.{Named => _, _}
import restrictions.relation.ast._
import restrictions.relation.RelationTrans

import scala.language.implicitConversions

case class RestrictionCompiler(wind: RelationDSL[UUID, WindF[Interpolator]],
                               tide: RelationDSL[UUID, TideF[Interpolator]],
                               tidalStream: RelationDSL[UUID, TidalStreamF[Interpolator]],
                               vesselTable: RelationDSL[EventPortcallId, Vessel],
                               berthTable: RelationDSL[URN, Berth]) {
  import RelationOrStream._

  def compile(rules: Restrict): RelationDSL[EventPortcallId, Result] =
    toRelationTransformer(rules.getStatements)(vesselTable)

  def locationContainsDest(location: Location): NamedLambda[Vessel, Boolean] =
    name { vessel: Vessel => location containsURN vessel.destination }

  def toResult(condition: Observation[Boolean]): NamedLambda[Boolean, Result] =
    ((allow: Boolean) => Result((allow, PrettyPrinter.pretty(condition)))).name("toResult")

  val anyVessel: NamedLambda[Vessel, Result] = name { _ => Allow }
  val isAllow: NamedLambda[Result, Boolean] = name { (_: Result).isAllow }
  val isDenied: NamedLambda[Result, Boolean] = name { !(_:Result).isAllow }

  private def observationToRelationTrans: Observation ~> RelTrans[EventPortcallId, Vessel, ?] =
    λ[Observation ~> RelTrans[EventPortcallId, Vessel, ?]] {
      case GetVessel() => x => RelationOrStream.relation(x)
      case GetWind() => (vessels: RelationDSL[EventPortcallId, Vessel]) => {
        val destinations: RelationDSL[EventPortcallId, URN] =
          vessels map { _.destination }
        val geometries: RelationDSL[URN, Geometry] =
          berthTable map { _.geo } filter { _.isDefined } map { _.get }
        val nearestWindSensor: RelationDSL[EventPortcallId, UUID] =
          (destinations joinForeign geometries) map { Distance.closestWindSensorUUID }
        val interpolatedWind: RelationDSL[EventPortcallId, InterpolatedWind] =
          nearestWindSensor joinForeign wind
        val windAtTimeOfInterest: RelationDSL[EventPortcallId, Wind] =
          vessels map { _.time } join interpolatedWind map { case (time, wind) =>
            WindF[Option](wind.velocity.predict(time), wind.direction.predict(time)).sequence
          } filter { _.isDefined } map { _.get }
        RelationOrStream.relation(windAtTimeOfInterest)
      }

      case GetTide() => (vessels: RelationDSL[EventPortcallId, Vessel]) => {
        val destinations: RelationDSL[EventPortcallId, URN] =
          vessels map { _.destination }
        val geometries: RelationDSL[URN, Geometry] =
          berthTable map { _.geo } filter { _.isDefined} map { _.get }
        val nearestTideSensor: RelationDSL[EventPortcallId, UUID] =
          (destinations joinForeign geometries) map { Distance.closestTideSensorUUID }
        val interpolatedTide: RelationDSL[EventPortcallId, InterpolatedTide] =
          nearestTideSensor joinForeign tide
        val tideAtTimeOfInterest: RelationDSL[EventPortcallId, Tide] =
          vessels map { _.time } join interpolatedTide map { case (time, tide) =>
            TideF[Option](
              tide.height.predict(time),
              tide.high_water.predict(time),
              tide.low_water.predict(time)
            ).sequence
          } filter { _.isDefined } map { _.get }
        RelationOrStream.relation(tideAtTimeOfInterest)
      }

      case GetTidalStream() => (vessels: RelationDSL[EventPortcallId, Vessel]) => {
        val destinations: RelationDSL[EventPortcallId, URN] =
          vessels map { _.destination }
        val geometries: RelationDSL[URN, Geometry] =
          berthTable map { _.geo } filter { _.isDefined } map { _.get }
        val nearestTidalStreamSensor: RelationDSL[EventPortcallId, UUID] =
          (destinations joinForeign geometries) map { Distance.closestTidalStreamSensorUUID }
        val interpolatedTidalStream: RelationDSL[EventPortcallId, InterpolatedTidalStream] =
          nearestTidalStreamSensor joinForeign tidalStream
        val tidalStreamAtTimeOfInterest: RelationDSL[EventPortcallId, TidalStream] =
          vessels map { _.time } join interpolatedTidalStream map { case (time, tidalStream) =>
            TidalStreamF[Option](tidalStream.rate.predict(time), tidalStream.direction.predict(time)).sequence
          } filter { _.isDefined } map { _.get }
        RelationOrStream.relation(tidalStreamAtTimeOfInterest)
      }

      case GetBerth() => (vessels: RelationDSL[EventPortcallId, Vessel]) => {
        RelationOrStream.relation(
          vessels map { _.destination } joinForeign berthTable
        )
      }
      case Void() => _ => RelationOrStream.stream(never)
      case Named(name, s) => observationToRelationTrans(s) andThen (_.mapStream(restrictions.stream.ast.named(name, _)))
      case unknown => throw new RuntimeException(s"Unknown DSL term: $unknown")
    }

  private def toRelationTransformer(statement: RestrictStatement): RelationTrans[EventPortcallId, Vessel, Result] =
    statement match {
      case RestrictLocation(_, Nil) => (_: RelationDSL[EventPortcallId, Vessel]) map { anyVessel }
      case RestrictLocation(location, r::rs) =>
        val body = NonEmptyList(r,rs)
          .map(toRelationTransformer)
          .reduce
        val filterVessels: RelationTrans[EventPortcallId, Vessel, Vessel] =
          _.filter(locationContainsDest(location))
        filterVessels >> body

      case Require(condition_) =>
        val condition: RelTrans[EventPortcallId, Vessel, Boolean] =
          condition_.foldApplicative(observationToRelationTrans)
        v => condition(v)
          .map(toResult(condition_))
          .fold(
            rel => rel,
            s => v zip s map snd
          )

      case When(_, Nil) => (_: RelationDSL[EventPortcallId, Vessel]) map { anyVessel }
      case When(condition_, r::rs) =>
        val body = NonEmptyList(r,rs)
          .map(toRelationTransformer)
          .reduce

        val condition: RelTrans[EventPortcallId, Vessel, Boolean] =
          condition_.foldApplicative(observationToRelationTrans)

        val f: RelationTrans[EventPortcallId, Vessel, Vessel] = v => {
          condition(v)
            .fold(r => v join r, s => v zip s)
            .filter(snd)
            .map(fst)
        }
        f >> body
    }
}

sealed trait RelationOrStream[K,A] {
  def mapStream(f: StreamDSL[A] => StreamDSL[A]): RelationOrStream[K,A]
  def mapRelation(f: RelationDSL[K,A] => RelationDSL[K,A]): RelationOrStream[K,A]
  def fold[B](f: RelationDSL[K,A] => B, g: StreamDSL[A] => B): B
}

case class Relation[K,A](relationDSL: RelationDSL[K,A]) extends RelationOrStream[K,A] {
  override def mapStream(f: StreamDSL[A] => StreamDSL[A]): RelationOrStream[K,A] = this
  override def mapRelation(f: RelationDSL[K,A] => RelationDSL[K,A]): RelationOrStream[K,A] = Relation(f(relationDSL))
  override def fold[B](f: RelationDSL[K, A] => B, g: StreamDSL[A] => B): B = f(relationDSL)
}

case class Stream[K,A](streamDSL: StreamDSL[A]) extends RelationOrStream[K,A] {
  override def mapStream(f: StreamDSL[A] => StreamDSL[A]): RelationOrStream[K,A] = Stream(f(streamDSL))
  override def mapRelation(f: RelationDSL[K,A] => RelationDSL[K,A]): RelationOrStream[K,A] = this
  override def fold[B](f: RelationDSL[K, A] => B, g: StreamDSL[A] => B): B = g(streamDSL)
}

object RelationOrStream {
  trait RelTrans[K,A,B] extends (RelationDSL[K, A] => RelationOrStream[K, B])
  object RelTrans {
    implicit def toRelTrans[K,A,B](f: RelationDSL[K,A] => RelationOrStream[K,B]): RelTrans[K,A,B] =
      (v1: RelationDSL[K,A]) => f.apply(v1)
  }

  def stream[K,A](streamDSL: StreamDSL[A]): RelationOrStream[K,A] =
    Stream(streamDSL)

  def apply[K,A](streamDSL: StreamDSL[A]): RelationOrStream[K,A] =
    Stream(streamDSL)

  def relation[K,A](relationDSL: RelationDSL[K,A]): RelationOrStream[K,A] =
    Relation(relationDSL)

  def apply[K,A](relationDSL: RelationDSL[K,A]): RelationOrStream[K,A] =
    Relation(relationDSL)


  trait RelationOrStreamApplicative[K] extends Applicative[RelationOrStream[K,?]] {
    override def pure[A](x: A) =
      Stream(Applicative[StreamDSL].pure(x))

    override def map[A, B](fa: RelationOrStream[K, A])(f: A => B): RelationOrStream[K, B] = fa match {
      case Relation(relationDSL) => Relation(relationDSL map f)
      case Stream(streamDSL) => Stream(streamDSL map f)
    }

    override def map2[A, B, Z](efa: RelationOrStream[K, A], efb: RelationOrStream[K, B])(f: (A, B) => Z) = (efa, efb) match {
      case (Relation(fa), Relation(fb)) =>
        Relation(Apply[RelationDSL[K,?]].map2(fa, fb)(f))
      case (Stream(fa), Stream(fb)) =>
        Stream(Apply[StreamDSL].map2(fa, fb)(f))
      case (Relation(relationA), Stream(streamB)) =>
        val applyTuple: NamedLambda[(A, B), Z] = f.tupled.name("{(a,b) => f(a,b)}")
        Relation(relationA zip streamB map applyTuple)
      case (Stream(streamA), Relation(relationB)) =>
        val applyTuple: NamedLambda[(B, A), Z] = ((t: (B,A)) => f(t._2, t._1)).name("{(a,b) => f(b,a)}")
        Relation(relationB zip streamA map applyTuple)
    }

    override def ap[A, B](eff: RelationOrStream[K, A => B])(efa: RelationOrStream[K, A]): RelationOrStream[K, B] =
      map2(eff, efa)((f: A => B, a: A) => f(a))
  }
  implicit def relationOrStreamApplicative[K]: Applicative[RelationOrStream[K, ?]] = new RelationOrStreamApplicative[K] {}

  trait RelationOrStreamTransApplicative[K,X] extends Applicative[RelTrans[K,X,?]] {
    override def pure[A](x: A): RelTrans[K,X,A] =
      _ => Applicative[RelationOrStream[K,?]].pure(x)

    override def map[A,B](fa: RelTrans[K, X, A])(f: A => B): RelTrans[K,X,B] =
      x => { fa(x) map f }

    override def ap[I, O](ff: RelTrans[K, X, I => O])(fa: RelTrans[K, X, I]): RelTrans[K,X,O] =
      x => { ff(x) ap fa(x) }

    override def map2[A, B, Z](fa: RelTrans[K, X, A], fb: RelTrans[K, X, B])(f: (A, B) => Z): RelTrans[K, X, Z] =
      x => { Applicative[RelationOrStream[K, ?]].map2(fa(x), fb(x))(f) }
  }
  implicit def RelTransApplicative[K,X]: Applicative[RelTrans[K,X,?]] = new RelationOrStreamTransApplicative[K,X] {}
}
