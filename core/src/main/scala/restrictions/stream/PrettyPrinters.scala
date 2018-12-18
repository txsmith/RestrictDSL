package restrictions.stream

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import restrictions.relation.{ast => relation}
import restrictions.ast._
import restrictions.relation.ast.{RelationDSL, ToRelation}
import restrictions.stream.ast._
import restrictions.stream.{ast => streamDSL}
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter

object PrettyPrinters {

  object PrettyPrinter extends PrettyPrinter {
    override val defaultIndent = 2

    def pretty(statement: StreamDSL[_]): String = layout(show(statement), 65)

    def show(statement: StreamDSL[_]): Doc = statement match {
      case Never() => text("never")
      case Now(v) => text("now") <> parens(value(v))
      case FromAkka(_) => text("fromAkka")
      case FromSeq(seq) => text("fromSeq") <> parens(value(seq))
      case streamDSL.Map(xs, f) => show(xs) <> nest(linebreak <> dot <> text("map") <> parens(value(f)))
      case streamDSL.Filter(xs, p) => show(xs) <> nest(linebreak <> dot <> text("filter") <> parens(value(p)))
      case streamDSL.Zip(xs, Now(r)) if r.isInstanceOf[RestrictStatementF[_]] => show(xs)
      case streamDSL.Zip(xs, ys) => show(xs) <> nest(linebreak <> dot <> text("zip") <> parens(nest(linebreak <> show(ys))))
      case Merge(xs, ys) => show(xs) <> nest(linebreak <> dot <> text("merge") <> parens(nest(linebreak <> show(ys))))
      case streamDSL.Named(name, xs) => text(name) <> parens(show(xs))
    }
  }

  object HTMLPrinter extends PrettyPrinter {
    override val defaultIndent = 2

    def pretty(statement: StreamDSL[_]): String =
      layout(show(statement), 65)

    def show(statement: StreamDSL[_]): Doc = statement match {
      case Never() => div(text("never"), "constant")
      case Now(v) if v.isInstanceOf[RestrictStatementF[_]] => div(text("now") <> parens(text("RestrictStatement")), "constant")
      case Now(v) => div(text("now") <> parens(value(v)), "constant")
      case FromAkka(_) => div(text("akka"), "constant")
      case streamDSL.Map(xs, f) => div(show(xs) <@> div(text("map") <> parens(value(f)), "f"), "map")
      case streamDSL.Filter(xs, p) => div(show(xs) <@> div(text("filter") <> parens(value(p)), "f"), "filter")
      case streamDSL.Zip(xs, ys) => div(div(show(xs) <@> show(ys)), "join")
      case streamDSL.Lookup(xs, ys) => div(div(show(xs) <@> show(ys)), "join relation")
      case Merge(xs, ys) => div(div(show(xs) <@> show(ys)), "merge")
      case streamDSL.Named(name, xs) => div(show(xs) <@> div(name, "name"), "named")
      case Changelog(xs) => div(show(xs) <@> div(text("changelog"), "f"), "changelog")
    }

    def pretty(statement: RelationDSL[_, _]): String =
      layout(show(statement), 65)

    def show(statement: RelationDSL[_, _]): Doc = statement match {
      case ToRelation(s) => div(show(s) <@> div(text("toRelation"), "f")  , "toRelation")
      case relation.Map(xs, f) => div(show(xs) <@> div(text("map") <> parens(value(f)), "f"), "map relation")
      case relation.Filter(xs, p) => div(show(xs) <@> div(text("filter") <> parens(value(p)), "f"), "filter relation")
      case relation.InnerJoin(xs, ys) => div(div(show(xs) <@> show(ys)), "join relation")
      case relation.OuterJoin(xs, ys) => div(div(show(xs) <@> show(ys)), "outerJoin relation")
      case relation.JoinValue(xs, ys) => div(div(show(xs) <@> show(ys)), "foreignJoin relation")
      case relation.Zip(rel, stream) => div(div(show(rel) <@> show(stream)), "zip relation")
    }

    def div(d: Doc, cls: String = ""): Doc =
      text(s"""<div class="$cls">""") <> nest(line <> d) <@> text("</div>")


    def span(d: Doc): Doc = text("<span>") <> d <> text("</span>")
  }

  object ASTPrinter extends PrettyPrinter {
    override val defaultIndent = 2

    def pretty(statement: StreamDSL[_]): String = layout(show(statement), 65)

    def show(statement: StreamDSL[_]): Doc = statement match {
      case Never() => text("Never()")
      case Now(v) => text("Now") <> parens(value(v))
      case FromAkka(_) => text("<akka stream>")
      case FromSeq(seq) => text("FromSeq") <> parens(value(seq))
      case streamDSL.Map(xs, f) => text("Map") <> parens(show(xs) <> char(',') <+> showLambda(f))
      case streamDSL.MapConcat(xs, f) => text("MapConcat") <> parens(show(xs) <> char(',') <+> showLambda(f))
      case streamDSL.Filter(xs, p) => text("Filter") <> parens(show(xs) <> char(',') <+> showLambda(p))
      case streamDSL.Zip(xs, ys) => text("Zip") <> parens(show(xs) <> char(',') <+> show(ys))
      case streamDSL.Lookup(xs, ys) => text("JoinRelation") <> parens(show(xs) <> char(',') <+> show(ys))
      case Merge(xs, ys) => text("Merge") <> parens(show(xs) <> char(',') <+> show(ys))
      case streamDSL.Named(name, xs) => text("Named") <> parens(text(name) <> char(',') <+> show(xs))
      case Changelog(relation) => text("Changelog") <> parens(show(relation))
    }

    def pretty(statement: RelationDSL[_,_]): String = layout(show(statement), 65)

    def show(statement: RelationDSL[_,_]): Doc = statement match {
      case ToRelation(s) => text("ToRelation") <> parens(show(s))
      case relation.Map(xs, f) => text("Map") <> parens(show(xs) <> char(',') <+> showLambda(f))
      case relation.Filter(xs, p) => text("Filter") <> parens(show(xs) <> char(',') <+> showLambda(p))
      case relation.InnerJoin(xs, ys) => text("JoinRelation") <> parens(show(xs) <> char(',') <+> show(ys))
      case relation.OuterJoin(xs, ys) => text("OuterJoin") <> parens(show(xs) <> char(',') <+> show(ys))
      case relation.JoinValue(xs, ys) => text("JoinValue") <> parens(show(xs) <> char(',') <+> show(ys))
      case relation.Zip(xs, ys) => text("CrossJoinStream") <> parens(show(xs) <> char(',') <+> show(ys))
    }

    def showLambda[A,B](f: A => B): Doc = value(f)
  }

  val htmlDirectory = "~/Documents/notes/stream-trees"

  def saveToFile(term: StreamDSL[_]): Unit = saveToFile(PrettyPrinters.HTMLPrinter.pretty(term))
  def saveToFile(term: RelationDSL[_, _]): Unit = saveToFile(PrettyPrinters.HTMLPrinter.pretty(term))
  private def saveToFile(termHtml: String): Unit = {
    val directory = Paths.get(System.getProperty("user.home"), "Documents", "notes", "stream-trees")
    val filePath = directory.resolve("tree.html")
    if (!Files.exists(directory))
      Files.createDirectory(directory)

    Files.write(filePath, html(termHtml).getBytes(StandardCharsets.UTF_8))
  }

  def html(s: String): String =
    s"""
       |<html>
       |<style>
       |
       |
       |      /* SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB
       |      --------- ------- ---- -------  ----------- ---------- ----------- -----------
       |      base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
       |      base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
       |      base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46
       |      base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51
       |      base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
       |      base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
       |      base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93
       |      base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
       |      yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71
       |      orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
       |      red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
       |      magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
       |      violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
       |      blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
       |      cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
       |      green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60 */
       |
       |      body {
       |          color: #93a1a1;
       |          background-color: #002b36;
       |          font-family: "Fira Code", monospace;
       |          font-size: 9px;
       |      }
       |      body > * {
       |          display: inline-block !important;
       |      }
       |
       |      .constant, .named {
       |          text-align: center;
       |          border: 1px dotted #93a1a1;
       |          padding: 5px;
       |      }
       |      .named > * {
       |          border: none;
       |      }
       |
       |      .map > div:first-child, .changelog > div:first-child, .toRelation > div:first-child,
       |      .filter > div:first-child, .join > div:first-child, .merge > div:first-child,
       |      .crossJoin > div:first-child, .zip > div:first-child, .outerJoin > div:first-child {
       |          flex-grow: 1;
       |      }
       |      .map, .filter, .join, .crossJoin, .zip, .outerJoin, .merge, .changelog, .toRelation {
       |          display: flex;
       |          flex-flow: column;
       |      }
       |
       |      .changelog > .f {
       |        color: #002b36;
       |        background-color: #d33682;
       |      }
       |      .toRelation > .f {
       |        color: #002b36;
       |        background-color: #2aa198;
       |      }
       |
       |      .relation::after,
       |      .relation > .f {
       |        filter: hue-rotate(90deg);
       |      }
       |      .map > .f {
       |        color: #002b36;
       |        background-color: #b58900;
       |      }
       |      .filter > .f {
       |        color: #002b36;
       |        background-color: #859900;
       |      }
       |      .merge::after {
       |        color: #002b36;
       |        background-color: #268bd2;
       |      }
       |      .join::after, .crossJoin::after, .outerJoin::after , zip::after {
       |        color: #002b36;
       |        background-color: #cb4b16;
       |      }
       |
       |      .filter > .f,
       |      .changelog > .f,
       |      .toRelation > .f,
       |      .map > .f ,
       |      .merge::after,
       |      .crossJoin::after,
       |      .zip::after,
       |      .outerJoin::after,
       |      .join::after {
       |          padding: 3px;
       |          text-align: center;
       |          /* border: 1px solid #93a1a1;  */
       |          height: 12px;
       |          line-height: 12px;
       |          margin-bottom: 5px;
       |          display: block;
       |      }
       |      .merge > div, .join > div,
       |      .crossJoin > div, .zip > div, .outerJoin > div {
       |          display: flex;
       |      }
       |      .merge > div > div:first-child,
       |      .join > div > div:first-child,
       |      .crossJoin > div > div:first-child,
       |      .zip > div > div:first-child,
       |      .outerJoin > div > div:first-child {
       |          margin-right: 8px;
       |      }
       |      .merge::after {
       |          content: "merge";
       |      }
       |      .join::after {
       |          content: "join";
       |      }
       |      .crossJoin::after {
       |          content: "crossJoin";
       |      }
       |      .zip::after {
       |          content: "zip";
       |      }
       |      .outerJoin::after {
       |          content: "outerJoin";
       |      }
       |</style>
       |<body>
       |  $s
       |</body>
       |</html>
       |
     """.stripMargin

}
