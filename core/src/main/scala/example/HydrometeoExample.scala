package example

import java.time.ZonedDateTime

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings, Supervision}
import play.api.Logger
import restrictions.compilers.StreamAkkaCompiler
import restrictions.pronto.hydrometeo.HydroMeteoTables
import restrictions.domain._
import restrictions.domain.external._
import restrictions.pronto.hydrometeo.HydroMeteoTables.HydroMeteo

import scala.concurrent.ExecutionContextExecutor

object HydrometeoExample extends App {

  implicit val actorSys: ActorSystem = ActorSystem("HydroMeteoTest")
  implicit val logger: Logger = Logger(getClass)
  implicit val loggingMaterializer = ActorMaterializer(ActorMaterializerSettings(actorSys).withSupervisionStrategy { e =>
    logger.error("Exception", e)
    Supervision.Stop
  })
  implicit val ec: ExecutionContextExecutor = actorSys.dispatcher
  logger.warn("Starting HydroMeteo")

  val HydroMeteo(wind, tide, tidalStream, stopHydroMeteo) = HydroMeteoTables.getHydroMeteoTables

  val akkaSource = StreamAkkaCompiler.compile(tidalStream)
  val now = ZonedDateTime.now()
  akkaSource runForeach {
    case Update(locationUUID, tidalStream) => logger.warn(s"${locationUUID.toString} ${tidalStream.rate.predict(now)}, ${tidalStream.direction.predict(now).map(_.toDegrees)}")
    case Delete(_) => logger.error("Unexpected delete from HydrometeoTables")
  } recover { case exception =>
    logger.error("Exception", exception)
    stopHydroMeteo.cancel()
    actorSys.terminate()
  }
}
