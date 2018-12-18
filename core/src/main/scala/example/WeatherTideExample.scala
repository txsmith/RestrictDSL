package example

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import restrictions.pronto.apis.WeatherTideAPI
import restrictions.pronto.apis.WeatherTideAPI._

import scala.concurrent.ExecutionContextExecutor

object WeatherTideExample extends App {
  val apiKey = scala.io.Source.fromResource("hydrometeo.key").mkString.stripLineEnd
  val hydroApi = WeatherTideAPI(apiKey)

  implicit val actorSys: ActorSystem = ActorSystem("WeatherTideExample")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val ec: ExecutionContextExecutor = actorSys.dispatcher

  hydroApi.fetch(WindSensor, TideSensor, TidalStreamSensor) foreach {
    _.fold(
      msg => throw new RuntimeException(msg),
      processMeasurements)
  }

  def processMeasurements(allMeasurements: Vector[Measurement]): Unit = {
    println("Received measurements")
    allMeasurements flatMap {
      case measurement =>
        val location = measurement.location
        List((location.uuid.toString, measurement.measurementCode))
    } groupBy(_._1) foreach {
      case (uuid, codes) => println(s"$uuid: ${codes.map(_._2)}")
    }

    actorSys.terminate()
  }
}
