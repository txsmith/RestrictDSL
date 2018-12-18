package restrictions.pronto.kafka

import java.time.ZonedDateTime

import akka.NotUsed
import akka.stream.Attributes
import akka.stream.scaladsl.Flow
import pronto.domain.{Interval, MMSI, MovementType, ProntoVisit, UNLOCODE, URN}
import restrictions.domain.external._
import restrictions.pronto.apis.AisApi
import cats._
import cats.implicits._
import play.api.Logger

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

object VisitToVessel {

  /**
    * Accumulate part of the data needed to construct a Vessel
    */
  case class VesselFields(visit: ProntoVisit,
                          name: String,
                          mmsi: MMSI,
                          vesselType: VesselType,
                          beam: Length,
                          length: Length)

  case class VesselFieldsWithDirection(fields: VesselFields, berthVisitLocation: URN,
                                       movementType: MovementType,
                                       movementTime: ZonedDateTime)

  object VesselFieldsWithDirection {
    def inbound(fields: VesselFields, berthVisitLocation: URN, time: ZonedDateTime): VesselFieldsWithDirection =
      VesselFieldsWithDirection(fields, berthVisitLocation, Inbound, time)
    def outbound(fields: VesselFields, berthVisitLocation: URN, time: ZonedDateTime): VesselFieldsWithDirection =
      VesselFieldsWithDirection(fields, berthVisitLocation, Outbound, time)
  }

  private val (aisUser, aisKey) = scala.io.Source.fromResource("ais.key").span(_ != '\n')
  private val aisApi = AisApi(aisUser.mkString, aisKey.mkString.stripPrefix("\n"))

  def toVessel(implicit ec: ExecutionContext, logger: Logger): Flow[ProntoVisit, Vessel, NotUsed] = {
    import restrictions.utils.LogWhenEmpty._
    /**
      * (1) Pre-filter
      *
      * Only consider port calls that:
      *  - involve the Port of Rotterdam
      *  - have a name, MMSI, ship type, vessel master data and ARES data
      */
    val preFilter: Flow[ProntoVisit, VesselFields, NotUsed] =
      Flow[ProntoVisit] mapConcat { visit =>
        val portcallID = visit.id.id
        (for {
          vmd <- visit.vesselMasterData.logWhenEmpty(portcallID)
          if visit.port == UNLOCODE.NLRTM
          name <- vmd.name.logWhenEmpty(portcallID)
          mmsi <- vmd.mmsi.logWhenEmpty(portcallID)
          beam <- vmd.beam.logWhenEmpty(portcallID)
          length <- vmd.lengthOverall.logWhenEmpty(portcallID)
          vesselType <- vmd.internalUsageOnly.logWhenEmpty(portcallID) map { ares => VesselType(ares.ihsShip.shipCategoryCode) }
        } yield VesselFields(visit, name, mmsi, vesselType, beam.meters, length.meters)).toList
      }

    /**
      * (2) Classify the port call as currently inbound or outbound and store the currently relevant berth location.
      *
      * A vessel is considered inbound if the port call has a berth visit for which:
      *  - the start time is in the future and;
      *  - there is an inboundMovement defined
      *
      * A vessel is considered outbound if:
      *  - it is not inbound and;
      *  - there is an outboundMovement defined
      *
      *  Note that a vessel might be neither inbound or outbound.
      */
    val classifyInboundOutbound: Flow[VesselFields, VesselFieldsWithDirection, NotUsed] =
      Flow[VesselFields] mapConcat { case fields@VesselFields(visit, _, _, _ ,_ , _) =>
        val now = ZonedDateTime.now()
        val portcallID = visit.id.id

        val inboundBerthVisits: List[VesselFieldsWithDirection] = for {
          berthVisit <- visit.berthVisits.toList.logWhenEmpty(portcallID)
          Interval(startTime, _) <- berthVisit.timeframeAlongside.toList
          if startTime.isAfter(now)
          _ <- berthVisit.inboundMovement.toList
          (_, locationURN) <- berthVisit.locations
        } yield VesselFieldsWithDirection.inbound(fields, locationURN, startTime)

        val outboundBerthVisit: List[VesselFieldsWithDirection] = for {
          berthVisit <- visit.berthVisits.toList
          _ <- berthVisit.outboundMovement.toList
          Interval(_, leaveTime) <- berthVisit.timeframeAlongside.toList

          /**
           * Don't filter on leave time!
           * To detect when a port call should be removed from tables we also need to observe events where the visit
           * has already ended.
           */
          // if leaveTime.isAfter(now)
          (_, locationURN) <- berthVisit.locations
        } yield VesselFieldsWithDirection.outbound(fields, locationURN, leaveTime)

        val inboundOrOutbound = (inboundBerthVisits.headOption orElse outboundBerthVisit.headOption).toList
        inboundOrOutbound.logWhenEmpty(portcallID)
      }

    /**
      * (3) Fetch the vessel draught and construct a Vessel object with all the gathered fields.
      * AIS info is fetched in batches to prevent ripping through the APIs rate limit.
      */
    val makeVessel = Flow[VesselFieldsWithDirection].groupedWithin(Int.MaxValue, 1500.millisecond).mapAsyncUnordered(1) { inOrOutbounds =>
      Future {
        val vesselInfos = aisApi.vesselInfo(inOrOutbounds map { _.fields.mmsi }).sortBy(_.mmsi.id)
        inOrOutbounds sortBy (_.fields.mmsi.id) zip vesselInfos map {
          case (inOrOutbound, vesselInfo) =>
            val fields = inOrOutbound.fields
            Vessel(
              mmsi = fields.mmsi,
              name = fields.name,
              vesselType = fields.vesselType,
              direction = inOrOutbound.movementType,
              time = inOrOutbound.movementTime,
              draught = vesselInfo.maxDraught,
              beam = fields.beam,
              length = fields.length,
              destination = inOrOutbound.berthVisitLocation,
              portcallId = fields.visit.eventId
            )
        }
      }
    }.mapConcat(identity)

    preFilter via classifyInboundOutbound via makeVessel
  }
}
