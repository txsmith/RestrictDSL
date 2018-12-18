package restrictions.akkaUtils

import net.team2xh.onions.Themes.ColorScheme
import net.team2xh.onions.components.{FramePanel, Widget}
import net.team2xh.onions.utils.Varying
import net.team2xh.scurses.{Keys, Scurses}

case class TableRow(parent: FramePanel,
                    rowItems: Varying[Seq[Any]],
                    itemSizes: Varying[Seq[Int]],
                    columnMargin: Int = 3,
                    var action: () => Unit = () => {})
                (implicit screen: Scurses) extends Widget(parent, rowItems) {

  var enabled = true
  def focusable = enabled

  def drawText(foreground: Int, background: Int): Unit = {
    val line = rowItems.value zip itemSizes.value map {
      case (item, size) => item.toString + " " * (size - item.toString.length)
    }
    screen.put(0, 0, line.mkString(" " * columnMargin),
      foreground = foreground, background = background)
  }

  override def redraw(focus: Boolean, theme: ColorScheme): Unit = {
    drawText(theme.foreground(focus), theme.background(focus))
  }

  override def handleKeypress(keypress: Int): Unit = {
    if (keypress == Keys.ENTER || keypress == Keys.SPACE) action()
  }

  override def innerHeight: Int = 1
}
