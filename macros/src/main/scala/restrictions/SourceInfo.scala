package restrictions

import sourcecode.SourceValue

import scala.reflect.macros.blackbox
import scala.language.experimental.macros

case class SourceInfo(location: SourceLocation,
                      file: sourcecode.File,
                      enclosing: sourcecode.Enclosing,
                      name: sourcecode.Name,
                      fullName: sourcecode.FullName
                     )
object SourceInfo {
  implicit def allSourceInfo(implicit
                             location: SourceLocation,
                             file: sourcecode.File,
                             enclosing: sourcecode.Enclosing,
                             name: sourcecode.Name,
                             fullName: sourcecode.FullName
                       ): SourceInfo =
    SourceInfo(location, file, enclosing, name, fullName)
}

case class SourceLocation(line: Int, column: Int)
object SourceLocation {
  implicit def generate: SourceLocation = macro SourceInfoMacros.generateLocation
}

class SourceInfoMacros(val c: blackbox.Context) {
  import c.universe._

  def error(msg: String) = c.abort(c.enclosingPosition, msg)
  def error(enclosingPosition: Position, msg: String) = c.abort(enclosingPosition, msg)
  def warn(msg: String) = c.warning(c.enclosingPosition, msg)
  def warn(enclosingPosition: Position, msg: String) = c.warning(enclosingPosition, msg)

  def generateLogWithPrefix[A](extra: c.Expr[String]): Expr[A] = {
    val extraStr = extra.tree match {
      case Literal(Constant(_)) => extra.tree
      case _ => q""" ${extra.tree.toString} ++ " = " ++ ${extra.tree}"""
    }
    c.Expr[A](q"""${c.prefix.tree}.logWhenEmpty(${c.prefix.tree.children.head.children(1).toString()}, $extraStr)""")
  }

  def generateLocation: Expr[SourceLocation] = {
    val line = c.enclosingPosition.line
    val col = c.enclosingPosition.column

    c.Expr[SourceLocation](q"_root_.restrictions.SourceLocation($line, $col)")
  }
}
