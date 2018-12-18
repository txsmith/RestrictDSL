package restrictions.ast

import org.bitbucket.inkytonik.kiama.output.{ParenPrettyPrinter, PrettyExpression}
import higherkindness.droste.scheme.cata

object PrettyPrinter extends ParenPrettyPrinter {
  override val defaultIndent = 2

  def pretty(statement: RestrictStatement): String =
    cata(higherkindness.droste.Algebra[RestrictStatementF, String] { pretty }).apply(statement)

  def pretty(obs: Observation[_]): String =
    layout(toParenDoc(obs))

  def pretty(statement: RestrictStatementF[String]): String = layout(show(statement), 65)

  def show(statement: RestrictStatementF[String]): Doc = statement match {
    case RequireF(obs) => text("require") <> parens(nest(softbreak <> toParenDoc(obs)))
    case WhenF(condition, restrictions) => text("when") <> parens(toParenDoc(condition)) <+> body(restrictions)
    case RestrictLocationF(loc, restrictions) => text("restrict") <> parens(value(loc)) <+> body(restrictions)
  }

  override def toParenDoc(observation: PrettyExpression): Doc = observation match {
    case Constant(x) => value(x)
    case GetWind() => text("wind")
    case GetTide() => text("tide")
    case GetTidalStream() => text("tidalStream")
    case GetVessel() => text("vessel")
    case Named(n, _) => text(n)
    case Map(obs, f) => toParenDoc(obs) <> dot <> value(f)
    case Ap(fObs, aObs) => text("Ap") <> parens(nest(softbreak <> toParenDoc(fObs) <> comma <> softline <> toParenDoc(aObs)))
    case _ => super.toParenDoc(observation)
  }

  def body(l : List[String]): Doc =
    braces(nest(line <> vsep(l map string)) <> line)
}
