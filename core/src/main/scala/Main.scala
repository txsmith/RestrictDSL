import restrictions._
import restrictions.domain._
import restrictions.domain.external._
import restrictions.execution._

object Main extends App {
  import RestrictionActorSystem._

  val exampleA = new Restrict(Z100/9/21) {
    require (vessel.length <= 200.meters)
  }

  val exampleB = new Restrict(Z100/6/979) {
    import environment._

    when (vessel.direction is Inbound or vessel.direction === Outbound) {
      when (vessel.draught < 17.4.meters) {
        require (berth.draught >= vessel.draught + 30.centimeters)
      }
    }
  }

  val exampleC = new Restrict(any) {
    import environment._

    when (vessel.direction === Inbound) {
      when (vessel.draught >= 14.3.meters) {
        when (tidalStream.direction >= 180.degrees) {
          require (tidalStream.rate <= 1.knots)
        }
      }
      when (vessel.draught >= 19.meters) {
        require (tidalStream.direction < 180.degrees)
      }
    }
  }














  ExecuteRestrictions.execAndPublish(exampleA).onComplete(_ => actorSys.terminate())
}
