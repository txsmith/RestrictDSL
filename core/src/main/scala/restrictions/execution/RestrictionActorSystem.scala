package restrictions.execution

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.Logger

import scala.concurrent.ExecutionContextExecutor

object RestrictionActorSystem {
  implicit val actorSys: ActorSystem = ActorSystem("RestrictionActorSystem")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = actorSys.dispatcher
  implicit val logger: Logger = Logger(getClass)
}
