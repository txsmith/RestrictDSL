package restrictions.domain.external

import java.time.ZonedDateTime

case class Vessel(portcallId: pronto.domain.EventPortcallId,
                  mmsi: pronto.domain.MMSI,
                  name: String,
                  vesselType: restrictions.domain.external.VesselType,
                  direction: MovementType,
                  time: ZonedDateTime,
                  draught: Length,
                  beam: Length,
                  length: Length,
                  destination: pronto.domain.URN,
                 ) {
  override def toString: String = name
}

