package restrictions.domain

import java.util.UUID

import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import pronto.domain.JtsInterop._
import pronto.domain.{Geometry, Point, URN}

import scala.collection.immutable.Set

sealed trait Location {
  def containsURN(otherLocation: URN): Boolean

  def +(otherLocation: Location): Location =
    LocationSet(Set(this, otherLocation))

  def toURN: Set[URN]
}

case class LocationSet(locations: Set[Location]) extends Location {
  override def +(otherLocation: Location): Location = otherLocation match {
    case LocationSet(otherSet) => LocationSet(locations union otherSet)
    case _ => LocationSet(locations + otherLocation)
  }

  override def toString = locations.mkString(" + ")

  override def containsURN(otherLocation: URN): Boolean =
    locations.exists(l => l.containsURN(otherLocation))

  override def toURN: Set[URN] = locations.flatMap(_.toURN)
}

case class BerthLocation(identifier: BerthIdentifier) extends Location {
  override def toString: String = identifier.toString

  override def containsURN(urn: URN): Boolean =
    BerthLocation.fromURN(urn) map { _.identifier } exists { otherLocation =>
      identifier.countryCode == otherLocation.countryCode &&
      identifier.sectionCode == otherLocation.sectionCode &&
      (identifier.locationCode == any || identifier.locationCode == otherLocation.locationCode)
    }

  override def toURN = Set(URN.pronto(
    pronto.domain.URN_NAMESPACE_PORTMAPS,
    pronto.domain.Location.Berth.urnID,
    identifier.toString))
}
object BerthLocation {
  def fromURN(urn: URN): Option[BerthLocation] =
    BerthIdentifier.fromString(urn.parts.last) map { BerthLocation(_) }
}

case class HydroMeteoSensorLocation(name: String, uuid: UUID, geometry: Geometry) extends Location {
  override def containsURN(otherLocation: URN)= false
  override def toURN: Set[URN] = Set(URN.fromUUID(uuid))

  override def equals(obj: Any): Boolean = obj match {
    case HydroMeteoSensorLocation(_, otherUUID, _) => uuid == otherUUID
    case _ => false
  }
}

/**
  * Dummy value used for testing
  */
case object AnyLoc extends Location {
  override def containsURN(otherLocation: URN) = true

  override def toURN = Set(URN.pronto("pronto-restrict", "DUMMY"))
}

object Distance {
  val prontoSpatialRef = "EPSG:4326"
  val prontoCRS = CRS.decode(prontoSpatialRef, true)

  val windLocations: Seq[(Point, UUID)] = Seq(
      (Point(3.275074315328171,51.99780324887209), UUID.fromString("b85807b9-dccc-4ea6-b594-6a73bb81dade")),
      (Point(4.392926514336839,51.84328080451542), UUID.fromString("0688b4be-3aca-4f4c-af46-a028249e762e")),
      (Point(3.275074315328171,51.99780324887209), UUID.fromString("e25438a2-1f3f-4f30-be06-090984874ba0")),
      (Point(4.242096059406772,51.91189535935999), UUID.fromString("fcfc3b43-dbf7-4cbd-8b2c-d3d9a95dd410")),
      (Point(4.084127862517258,51.9866941718077), UUID.fromString("d650a58a-c1ff-4645-8493-a53c235d475d")),
      (Point(3.6684334389098283,51.92503290021363), UUID.fromString("86e2a739-f618-48a7-980b-daf21329a3e7")),
      (Point(4.08144329525315,51.98138775854527), UUID.fromString("8991c87d-7247-4ab9-ab71-b1d7e4d8a03a")),
      (Point(4.08144329525315,51.98138775854527), UUID.fromString("4eaa3d55-20c6-4ef5-a9b3-756283e0458c")),
      (Point(3.9773497978556103,51.97833445709245), UUID.fromString("743aa11d-1616-45f8-a0bc-cddcfd7029ea")),
      (Point(4.429837562342012,51.90440613969534), UUID.fromString("d56f5618-8468-4db4-b01b-4c5d3b527ee7")),
      (Point(4.312632840137583,51.89181500106026), UUID.fromString("1b9b9c4d-9cec-4765-81d3-f450a4886184")),
      (Point(3.6684334389098283,51.92503290021363), UUID.fromString("664f8d17-9196-4f00-b77a-b3f69f1d015b")),
      (Point(4.078787932151563,51.9646203689396), UUID.fromString("fffddc0b-322b-4f0d-b7fd-7ded73bbd752"))
    )

  val tideLocations: Seq[(Point, UUID)] = Seq(
    (Point(4.670715288492556, 51.819525268793114), UUID.fromString("a7a514d7-0996-4675-8f2c-32f7655011be")),
    (Point(4.628905656958464, 51.89072246507608), UUID.fromString("8c2f132-8097-4486-bb62-2fc6b6f92edb")),
    (Point(3.6684334389098283, 51.92503290021363), UUID.fromString("70c22cc-195f-4d4c-a71d-c2fe47018725")),
    (Point(4.0424657909338135, 51.92964685644895), UUID.fromString("d0a1899-a149-4141-9cb8-b8210a0c0aac")),
    (Point(4.2484759441060245, 51.917665382492544), UUID.fromString("4740b6f-fb8f-46cb-8bf6-82a2a0768aec")),
    (Point(4.451750447719744, 51.830406815061465), UUID.fromString("f8d3a02-bc15-461d-ac7d-27e8c545756f")),
    (Point(4.210665151869526, 51.90160700091598), UUID.fromString("e533f0e-f100-4f6b-947d-941cedca36d0")),
    (Point(4.033444570114547, 51.82702840253495), UUID.fromString("7a7d6a0-90ec-48eb-8771-50c82ad5b080")),
    (Point(4.464381703171275, 51.90643530605134), UUID.fromString("7752ab9-aa7d-47a0-b348-87034f4d8776")),
    (Point(4.851270842245542, 51.94309124055345), UUID.fromString("52ad54f-49d8-403e-b79b-61770332b0cc")),
    (Point(4.578469545642774, 51.91645322406084), UUID.fromString("d47bb3a-7bd6-49aa-bad0-bd359a8c7686")),
    (Point(3.8607654873075052, 51.863133566738654), UUID.fromString("7069a00-c00d-4605-b36f-e01232214e23")),
    (Point(4.8768245311351945, 51.806041073772974), UUID.fromString("670e400-f353-478c-9af0-6adca29115ef")),
    (Point(4.231179071526072, 51.4251176813162), UUID.fromString("32dfcb6-7bff-4a9e-9991-bb280c96bfa6")),
    (Point(3.275074315328171, 51.99780324887209), UUID.fromString("e41c6b5-0ba6-4e3d-8554-485e03ad17e4")),
    (Point(4.028371175243381, 51.96199909903923), UUID.fromString("fbf260b-3175-4001-8652-80abf1e7471c")),
    (Point(3.994955030211611, 51.96219254668458), UUID.fromString("e1f30aa-2fe0-47b6-86f4-6f63f90b5bb0")),
    (Point(3.275074315328171, 51.99780324887209), UUID.fromString("1e2370f-8b93-486f-aded-41f74c668728")),
    (Point(4.397417046836664, 51.682433651214126), UUID.fromString("0e3a3bc-c29d-4b19-9319-a3be0ef0e322")),
    (Point(4.421586375375823, 51.69504031546849), UUID.fromString("98c97d1-b55d-4170-8dd2-8d6d76a46737")),
    (Point(4.312543870492392, 51.86695965770047), UUID.fromString("d3d4ffa-5871-45c1-8516-828e72d975d6")),
    (Point(4.142799609002418, 51.65949079176629), UUID.fromString("6cc45df-932e-4b0b-a610-c3aadcc7f272")),
    (Point(4.2293047993002055, 51.889917604833165), UUID.fromString("fc506c0-29b9-420a-83d7-0281259be885")),
    (Point(4.499785714144889, 51.9202719756429), UUID.fromString("90cb40c-bf92-4c60-b42f-79a137f248d8")),
    (Point(4.0935613093205925, 51.963507333535425), UUID.fromString("f4d43d1-5425-4163-88ce-86e0b5028576")),
    (Point(4.128238637702707, 51.819723560229825), UUID.fromString("9c88a22-2466-4797-9780-a46d2de495ff")),
    (Point(4.119880987595917, 51.97757424530118), UUID.fromString("900ffa4-89c3-45df-8861-15e4f2592093")),
    (Point(4.311781763189174, 51.89095326157788), UUID.fromString("6b21c79-36d0-462d-8e48-b07fe6b83b30")),
    (Point(4.34904825864819, 51.899639189699386), UUID.fromString("1e6491e-4d57-4f17-8988-28179b5ab4aa")),
    (Point(4.622478172259246, 51.704703970619676), UUID.fromString("2a5a262-bc10-427e-865a-bfeb3e6ccdfa")),
    (Point(4.1588879240723235, 51.96016266684343), UUID.fromString("cf9a79d-e627-4d67-a281-7500ee2a3d18")),
    (Point(4.228530603434174, 51.894593280083775), UUID.fromString("275defc-aeef-4c1c-b4a7-c47668afd18e")),
    (Point(4.107184105031442, 51.93791137591469), UUID.fromString("cab5888-d1ee-4861-8bbc-52cb1d648df4")),
    (Point(4.518373516899046, 51.89946715029773), UUID.fromString("543f4a8-cd01-4b52-8915-f811c0fc277a")),
    (Point(3.6684334389098283, 51.92503290021363), UUID.fromString("838eb49-bf21-48c5-a1df-92644427f5b7")),
    (Point(4.419942422968027, 51.88881937304095), UUID.fromString("8d68520-d0e9-4ff0-ac73-38810d26ad3d")),
    (Point(4.333954155339891, 51.86166136907743), UUID.fromString("21e4a66-0273-45f9-914d-3edf1bc5ee22"))
  )

  /**
   * For Tidal Stream measurements we only take a select few sensors that are close to places
   * where vessels have to turn to get to their berth. This ensures that the only measurements
   * taken are for places where the tidal stream actually matters, which is in tight turns.
   */
  val tidalStreamLocations: Seq[(Point, UUID)] = Seq(
    (Point((4.3648360191367725,51.90070797190327)), UUID.fromString("61fd3cc5-63e8-4c12-827d-6d9bb9200555")), // Vulcaanhaven Nieuwe Maas
    (Point((4.302179469778963,51.89430235448699)), UUID.fromString("bbb795bf-6a61-498d-af98-1b1ec1832901")), // Botlek mond
    (Point((4.468009868052871,51.89956935514637)), UUID.fromString("0a818ff7-4462-4060-a148-c9048ec24877")), // Maashaven Mond
    (Point((4.41137083873868,51.90023622846236)), UUID.fromString("37aea64c-8703-45b6-a6a9-7461446c8e2b")), // Heysehaven Nieuwe Maas
    (Point((4.1286015632320145,51.962736936150115)), UUID.fromString("67b92ca1-fd54-4bae-9f19-e13a365e9305")), // Beneluxhaven mond
    (Point((4.380987521342966,51.898930967501265)), UUID.fromString("609a5ecc-d210-4336-8226-99dfff4ba7e7")), // Wiltonhaven mond
    (Point((4.165747189987114,51.94604033321514)), UUID.fromString("f9937385-0690-417d-9d65-09d508e36847")), // 5e Petroleumhaven
    (Point((4.341061199401488,51.89381543667659)), UUID.fromString("0244d618-e565-42cd-8875-18412231535a")), // 1e Pet mond
    (Point((4.3682885531610705,51.89827051183578)), UUID.fromString("d072d4e3-8a6c-4d12-b3a5-accb04d13a90")), // 2e Pet mond
    (Point((4.323168893871413,51.89536265499367)), UUID.fromString("1066203b-25c9-4e90-8a01-7d42feefa5a6")), // Zevenmanshaven Nieuwe Maas
    (Point((4.0875006146988975,51.97134200996136)), UUID.fromString("0ceb6223-00ac-4e0b-86dc-f585bbd91904")), // Beerkanaal mond
    (Point((4.434440598623762,51.90183140387252)), UUID.fromString("0a7c6096-6bee-4191-ab19-6b6dc0f88d06")), // Waalhaven Nieuwe Maas
  )
  def closestWindSensorUUID: Geometry => UUID =
    closestUUID(windLocations)

  def closestTideSensorUUID: Geometry => UUID =
    closestUUID(tideLocations)

  def closestTidalStreamSensorUUID: Geometry => UUID =
    closestUUID(tidalStreamLocations)

  def closestUUID(locations: Seq[(Point, UUID)])(location: Geometry): UUID = {
    locations.map { l =>
      (JTS.orthodromicDistance(l._1.asJtsGeom.getCoordinate, location.asJtsGeom.getCoordinate, prontoCRS), l._2)
    }.minBy(_._1)._2
  }
}

