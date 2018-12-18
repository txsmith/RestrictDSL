package example

import pronto.domain.MMSI
import restrictions.pronto.apis.AisApi

object AISApiExample extends App {
  private val (aisUser, aisKey) = scala.io.Source.fromResource("ais.key").span(_ != '\n')
  private val aisApi = AisApi(aisUser.mkString, aisKey.mkString.stripPrefix("\n"))

  val vesselInfo = aisApi.vesselInfo(MMSI("310132000"))
  println(vesselInfo)
}
