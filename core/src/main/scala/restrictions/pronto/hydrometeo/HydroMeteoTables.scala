package restrictions.pronto.hydrometeo

import java.util.UUID

import akka.NotUsed
import akka.actor.{ActorSystem, Cancellable}
import akka.stream._
import akka.stream.scaladsl._

import cats.implicits._

import play.api.Logger
import restrictions.domain._
import restrictions.pronto.apis.WeatherTideAPI
import restrictions.relation.ast.{RelationDSL, RowUpdate}
import restrictions.stream.ast.{Map => _, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

object HydroMeteoTables {
  type AkkaStream[A] = Source[A, NotUsed]

  val apiKey = scala.io.Source.fromResource("hydrometeo.key").mkString.stripLineEnd
  val hydroApi = WeatherTideAPI(apiKey)

  case class HydroMeteo(wind: RelationDSL[UUID, InterpolatedWind],
                        tide: RelationDSL[UUID, InterpolatedTide],
                        tidalStream: RelationDSL[UUID, InterpolatedTidalStream],
                        cancalUpdate: Cancellable)

  def getHydroMeteoTables[T](implicit
              ec: ExecutionContext,
              mat: ActorMaterializer,
              actorSystem: ActorSystem,
              logger: Logger
           ): HydroMeteo = {
    import WeatherTideAPI._

    val (windEventQueue, windEvents) = newQueueSource[RowUpdate[UUID, InterpolatedWind]]
    val (tideEventQueue, tideEvents) = newQueueSource[RowUpdate[UUID, InterpolatedTide]]
    val (tidalStreamQueue, tidalStreamEvents) = newQueueSource[RowUpdate[UUID, InterpolatedTidalStream]]

    def offerEvent[A](queue: SourceQueueWithComplete[RowUpdate[UUID, A]],
                      location: HydroMeteoSensorLocation,
                      event: A): Future[Unit] =
      queue.offer(RowUpdate(location.uuid, event)).flatMap {
        case QueueOfferResult.Failure(cause) => Future.failed(cause)
        case _ => Future.unit
      }

    def publishMeasurements(measurements: List[Measurement]): Future[Unit] = {
      val byLocationAndType: Map[HydroMeteoSensorLocation, Map[String, List[Measurement]]] =
        measurements.groupBy(_.location).mapValues(_.groupBy {
            case wind.velocity(_,_) => "wind.velocity"
            case wind.direction(_,_) => "wind.direction"
            case tide.height(_,_) => "tide.height"
            case tide.lowWater(_,_) => "tide.lowWater"
            case tide.highWater(_,_) => "tide.highWater"
            case tide.stream.rate(_, _) => "tide.stream.rate"
            case tide.stream.direction(_, _) => "tide.stream.direction"
          }
        )

      var windInterpolators: Map[HydroMeteoSensorLocation, InterpolatedWind] = null
      windInterpolators = byLocationAndType mapFilter { measurementsByType =>
        for {
          velocities <- measurementsByType.get("wind.velocity")
          velocityInterpolator = Interpolator(velocities).map(_.asInstanceOf[VelocityMeasurement].value)
          directions <- measurementsByType.get("wind.direction")
          directionInterpolator = Interpolator(directions).map(_.asInstanceOf[AngleMeasurement].value)
        } yield WindF[Interpolator](velocityInterpolator, directionInterpolator)
      }

      val tidalStreamInterpolators: Map[HydroMeteoSensorLocation, InterpolatedTidalStream] = byLocationAndType mapFilter { measurementsByType =>
        for {
          rates <- measurementsByType.get("tide.stream.rate")
          velocityInterpolator = Interpolator(rates).map(_.asInstanceOf[VelocityMeasurement].value)
          directions <- measurementsByType.get("tide.stream.direction")
          directionInterpolator = Interpolator(directions).map(_.asInstanceOf[AngleMeasurement].value)
        } yield TidalStreamF[Interpolator](velocityInterpolator, directionInterpolator)
      }

      val tideInterpolators: Map[HydroMeteoSensorLocation, InterpolatedTide] = byLocationAndType mapFilter { measurements =>
        for {
          height <- measurements.get("tide.height")
          heightInterpolator = Interpolator(height).map(_.asInstanceOf[LengthMeasurement].value)
          lowWater <- measurements.get("tide.lowWater")
          lowWaterInterpolator = Interpolator(lowWater).map(_.asInstanceOf[LengthMeasurement].value)
          highWater <- measurements.get("tide.highWater")
          highWaterInterpolator = Interpolator(highWater).map(_.asInstanceOf[LengthMeasurement].value)
        } yield TideF[Interpolator](heightInterpolator, highWaterInterpolator, lowWaterInterpolator)
      }

      for {
        _ <- windInterpolators.toList traverse { case (loc, windF) => offerEvent(windEventQueue, loc, windF) }
        _ <- tideInterpolators.toList traverse { case (loc, tideF) => offerEvent(tideEventQueue, loc, tideF) }
        _ <- tidalStreamInterpolators.toList traverse { case (loc, streamF) => offerEvent(tidalStreamQueue, loc, streamF) }
      } yield ()
    }


    val cancelUpdate = actorSystem.scheduler.schedule(0.seconds, 3.minutes) {
      logger.warn("Fetching hydro meteo measurements")
      (for {
          apiResponse <- hydroApi.fetch(WindSensor, TideSensor, TidalStreamSensor)
          _ = if (apiResponse.isLeft) {
            logger.error(s"Hydro-meteo api error: ${apiResponse.left.get}")
          }
          measurements: Vector[Measurement] = apiResponse.right.get
          result <- publishMeasurements(measurements.toList) onError { case exception =>
            logger.error(s"Error while publishing hydro-meteo events: $exception")
            throw exception
          }
      } yield result).recover { case e: Throwable =>
        windEventQueue.fail(e)
        tideEventQueue.fail(e)
        tidalStreamQueue.fail(e)
      }
    }

    HydroMeteo(
      fromAkka(windEvents).toRelation,
      fromAkka(tideEvents).toRelation,
      fromAkka(tidalStreamEvents).toRelation,
      cancelUpdate)
  }

  private def newQueueSource[A](implicit mat: Materializer): (SourceQueueWithComplete[A], Source[A, NotUsed]) =
    Source.queue[A](10000, OverflowStrategy.backpressure).preMaterialize()
}
