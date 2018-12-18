import scala.util.{ Failure, Success, Try }
import shapeless._

trait FromRow[L <: HList] {
  def apply(row: List[String]): Try[L]
}

object FromRow {
  import HList.ListCompat._

  implicit val hnilFromRow: FromRow[HNil] = {
    case Nil => Success(HNil)
    case _ => Failure(new RuntimeException("No more rows expected"))
  }

  implicit def hconsFromRow[H: Read, T <: HList: FromRow]: FromRow[H :: T] = {
    case h :: t => for {
      hv <- Read[H].reads(h)
      tv <- implicitly[FromRow[T]].apply(t)
    } yield hv :: tv
    case Nil => Failure(new RuntimeException("Expected more cells"))
  }
}