package restrictions.akkaUtils

import java.util.{Timer, TimerTask}

import net.team2xh.onions.components.Frame
import net.team2xh.onions.components.widgets.{Label, Separator}
import net.team2xh.onions.utils.{TextWrap, Varying}
import net.team2xh.scurses.Scurses

import scala.collection.{Seq, mutable}
import scala.language.implicitConversions

object TableUI {

  implicit def seqVarying[T](values: Seq[T]): Varying[Seq[T]] = new Varying(values)

  def apply[K,V <: Product](headerLabels: Seq[String],
                            table: mutable.Map[K,V],
                            uiAction: (K,V) => Unit = (_: K, _: V) => {}): Unit = {
    val clockTimer = new Timer()

    Scurses { implicit screen =>
      val frame = Frame(Option.empty)
      val itemSizes: Varying[Seq[Int]] = headerLabels map { _.length }
      val headerRow = TableRow(frame.panel, headerLabels, itemSizes)
      Separator(frame.panel)

      val margins = 3
      val rows = (1 to frame.height-2*margins).foldLeft(Vector.empty[TableRow]) {
        case (acc, _) => acc :+ TableRow(frame.panel, Seq("-"), itemSizes)
      }
      val bottomLabel = Label(frame.panel, "all 0 results shown", TextWrap.CENTER)

      clockTimer.scheduleAtFixedRate(new TimerTask {
        override def run(): Unit = {
          table.toList.zip(rows).foreach {
            case ((key, tuple), row) =>
              val values = key +: tuple.productIterator.toSeq
              itemSizes := values zip itemSizes.value map {
                case (value, prevSize) => value.toString.length max prevSize
              }
              row.action = () => uiAction(key,tuple)
              row.rowItems := values
          }
          if (rows.size < table.size) {
            bottomLabel.text := s"${rows.size} out of ${table.size} results shown"
          } else {
            bottomLabel.text := s"all ${table.size} results shown"
          }
          headerRow.redraw()
          frame.redraw()
        }
      }, 1000, 1000)

      frame.show()
    }

    clockTimer.cancel()
  }
}
