package example

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.Logger
import pronto.domain.{Berth, URN}
import restrictions.compilers.StreamAkkaCompiler
import restrictions.domain._
import restrictions.domain.external.Delete
import restrictions.pronto.kafka.NauticalLocationsTableKafka
import restrictions.relation.ast.{RelationDSL, Update}

object NauticalLocationsExample extends App {
  implicit val actorSys = ActorSystem("NauticalLocationsExample")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec = actorSys.dispatcher
  implicit val logger: Logger = Logger(getClass)

  val berth = Z100/8/any
  val table: RelationDSL[URN, Berth] =
    NauticalLocationsTableKafka.getNauticalLocationsTable
    .filter(loc => berth containsURN loc.urn)

  println(s"Looking for berth: $berth, this takes about 30 seconds")
  StreamAkkaCompiler.compile(table).runForeach {
    case Update(urn, loc) => println(s"Found: $urn, draught: ${loc.draught} meters")
    case Delete(urn) => println(s"Deleted: $urn")
  }
}
