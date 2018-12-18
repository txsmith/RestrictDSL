import shapeless._
import scala.util.Try

trait RowParser[A] {
  def apply[L <: HList](row: List[String])(implicit
                                           gen: Generic.Aux[A, L],
                                           fromRow: FromRow[L]): Try[A] =
    fromRow(row).map(gen.from)
}
object RowParser {
  def rowParserFor[A]: RowParser[A] = new RowParser[A] {}
}

