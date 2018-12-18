import cats._
import cats.implicits._
import com.opencsv.{CSVParserBuilder, CSVReader, CSVReaderBuilder}

import scala.collection.JavaConverters._
import scala.collection.immutable.List._
import scala.io.Source.fromInputStream
import restrictions.domain._
import restrictions.domain.external._
import restrictions.ast._
import Strategy._

/**
 * Tranansform an entire CSV file of restrictions into a value of type RestrictStatement,
 * in such a way that the resulting AST exactly describes the rules in the original file,
 * but minimizes the size of the AST.
 *
 * This is an approximation. We attempt to merge redundant, overlapping and contradicting
 * rules as best we can, but this remains imperfect. When reading waalhaven.csv, you should
 * get a little over 200 lines of minimized output.
 *
 * Minimizing an AST happens with rewrite rules. The Kiama library was specifically made
 * for this, but we don't use it here due to its bugginess. Instead I've written a very
 * a very small library myself (Strategy.scala)
 *
 * Helpful background knowledge:
 *  - Kiama, a library for building composable AST transformations Specifically:
 *      - https://bitbucket.org/inkytonik/kiama/src/master/wiki/Research.md
 *      - https://speakerdeck.com/inkytonik/embedding-a-rewriting-dsl-in-scala
 */
// TODO: For some reason the output is not as minimal as it used to be, inspect which rules don't fire consistently.
object Main extends App {
  import RowParser._
  import RuleParser._

  // First read the actual file
  private val reader: CSVReader = new CSVReaderBuilder(fromInputStream(getClass.getResourceAsStream("waalhaven.csv")).reader())
    .withSkipLines(1)
    .withCSVParser(new CSVParserBuilder().withSeparator(';').build())
    .build()

  // Construct a parser and read each CSV line into a value of type UnknownRUle
  val parser = rowParserFor[UnknownRule]
  val uRules: List[UnknownRule] = reader.readAll.asScala.map(r => parser(r.toList)).toList.sequence.get

  import Strategies._

  /**
   * This is the complete strategy for reducing redundancies in a RestrictStatement AST.
   * All individual rules are defined below in Strategies.
   *
   * Some strategies are repeated because one may trigger another.
   * This is also the reason for first traversing top down, then bottom up:
   * some rewrites may trigger new ones, but only when applied in the right order (e.g. children first).
   */
  val simplify: Strategy[RestrictStatementF] =
    topdown ( // Traverse the AST from top to bottom, at each level:
      sortBy(_.toString) <* // Sort the subexpressions alphabetically,
      repeat (mergeRestrict + mergeWhen + mergeRedundantRequires + removeDuplicate) <* // keep merging rules until no merges can be done
      attempt ( // then try each of these strategies once until one succeeds.
        removeTautology + extractCommonSubexpr + mergeFreshWaterAllowance +
        mergeSimilarRequires + shortenWhen
      )
    ) <*
    bottomup ( // Then, from the bottom up, at each level,
      sortBy(treeSize) <* repeat (mergeImpliedRules) <* sortBy(_.toString) // Sort by tree size, merge rules that imply each other.
    )

  /**
   * Parse each individual row into a RestrictStatement, and make that into an unoptimized
   * restriction on Z100/6/any (i.e. Waalhaven)
   */
  val allRules: RestrictStatement = RestrictLocation(
    Z100/6/any,
    uRules map (parseRule(_)) map (RuleParser.toTotalAST(_))
  )

  /**
   * Then apply the simplify strategy and print the result.
   */
  val result = simplify.run(allRules)
  println(PrettyPrinter.pretty(result))
}

/**
 * This module defines all the individual rules that can be used to reduce the size of a RestrictStatement,
 * while hopefully not changing it's outcome.
 *
 * The individual rules are Strategies and thus can be composed nicely as we've done above.
 */
object Strategies {
  import RuleParser.name

  /**
   * Common subexpression elimination (CSE).
   * Whenever two expressions have enough equal children,
   * we will extract the deduplicate these rules.
   */
  val extractCommonSubexpr: Strategy[RestrictStatementF] = foldChildren {
    case (
      RestrictLocation(l1, rs1),
      RestrictLocation(l2, rs2)
    ) if hasEnoughOverlap(rs1, rs2) =>
      val (rules1, rules2) = (rs1.toSet, rs2.toSet)
      val overlap = (rules1 & rules2).toList
      val uniqueRulesIn1 = (rules1 -- rules2).toList
      val uniqueRulesIn2 = (rules2 -- rules1).toList
      val r1 = if (uniqueRulesIn1.nonEmpty) List(RestrictLocation(l1, uniqueRulesIn1)) else Nil
      val r2 = if (uniqueRulesIn2.nonEmpty) List(RestrictLocation(l2, uniqueRulesIn2)) else Nil
      RestrictLocation(l1 + l2, overlap) :: (r1 ++ r2)
  }

  /**
   * If we see two rules where one is about the fresh water allowance,
   * and the other about depth of the berth, we can merge these two.
   */
  val mergeFreshWaterAllowance = foldChildren {
    case (
      Require(Equals(Named("fresh_water_allowance", _), Constant(percentage: Double))),
      Require(GreaterOrEq(Named("contract_depth", _), requiredDepth, ord))
    ) =>
      val depth = requiredDepth.asInstanceOf[Observation[Length]]
      List(Require(GreaterOrEq(name("contract_depth"), Constant(1 + percentage / 100) * depth, ord)))
  }

  /**
   * An expression that says:
   *   when (c) { require(false) }
   * Is just a verbose way of sayingL
   *   require(not(c))
   */
  val shortenWhen = rule[RestrictStatementF] {
    case When(c, List(Require(Constant(false)))) => Require(Not(c))
  }

  /**
   * If we look at the contents of waalhaven.csv, you'll see many rules that come down to:
   *   require(x >= y + z)
   *
   * Whenever two of these exist where x and y are equal, we can merge them.
   */
  val mergeRedundantRequires = foldChildren {
    case (
      Require(GreaterOrEq(lhsX, Plus(rhsX1, Constant(rhsX2), num), ord)),
      Require(GreaterOrEq(lhsY, Plus(rhsY1, Constant(rhsY2), _), _))
    ) if lhsX == lhsY && rhsX1 == rhsY1 =>
      List(Require(GreaterOrEq(lhsX, Plus(rhsX1, Constant(num.max(rhsX2, rhsY2)), num), ord)))
  }

  /**
   * If two rules are just the same, eliminate one of them.
   */
  val removeDuplicate = foldChildren {
    case (rule1, rule2) if rule1 == rule2 =>
      List(rule1)
  }

  /**
   * If a rule is always true, it will always be true...
   */
  val removeTautology = foldChildren {
    case (Require(Constant(true)), y) =>
      List(y)
  }

  /**
   * Merge two when-statements if either their condition or body is the same.
   */
  val mergeWhen = foldChildren {
    case (
      When(c1, rs1),
      When(c2, rs2)
    ) if rs1 == rs2 =>
      List(When(c1 || c2, rs1))
    case (
      When(c1, rs1),
      When(c2, rs2)
    ) if c1 == c2 =>
      List(When(c1, rs1 ++ rs2))
  }

  /**
   * Merge two restrictions if they talk about the same location,
   * or if they talk about different locations but have the same rules.
   */
  val mergeRestrict = foldChildren {
    case (
      RestrictLocation(l1, rs1),
      RestrictLocation(l2, rs2)
    ) if rs1 == rs2 =>
      List(RestrictLocation(l1 + l2, rs1))
    case (
      RestrictLocation(l1, rs1),
      RestrictLocation(l2, rs2)
    ) if l1 == l2 =>
      List(RestrictLocation(l1, rs1 ++ rs2))
  }

  /**
   * This one merges two seperate rules into one if they both talk about vessel beam or length.
   * Done mostly for aesthetics.
   */
  val mergeSimilarRequires = foldChildren {
    case (
      Require(LessOrEq(Named("vessel.beam",   Void()), x, ord1)),
      Require(LessOrEq(Named("vessel.length", Void()), y, ord2))
    ) =>
      List(Require(
        And(LessOrEq(Named("vessel.beam",   Void()), x, ord1),
          LessOrEq(Named("vessel.length", Void()), y, ord2))))
  }

  /**
   * Collapses two rules when one implies the other.
   * For example:
   *   require (vessel.length <= 100.meters)
   *   require (vessel.length <= 50.meters)
   *
   * Determining precisely when one rule implies the other is a bit tricky,
   * so this is also a heuristic that just won't trigger if there is no obvious implication.
   */
  val mergeImpliedRules = foldChildren {
    def isComplement[A](x: Observation[A], y: Observation[A]): Boolean = (x,y) match {
      case (a, Not(b)) if a == b => true
      case (Not(a), b) if a == b => true
      case (LessThan(a, b, _), GreaterOrEq(c, d, _)) if a == c && b == d => true
      case (GreaterOrEq(a, b, _), LessThan(c, d, _)) if a == c && b == d => true
      case _ => false
    }

    def implies[A](x: Observation[A], y: Observation[A]): Boolean = (x,y) match {
      case (GreaterOrEq(a, b, _), GreaterOrEq(c, d, _)) if a == c => isAtLeastAsLarge(b, d)
      case (LessOrEq(a, b, _), LessOrEq(c, d, _)) if a == c => isAtLeastAsLarge(d, b)
      case (And(a,b), c) => implies(a,c) || implies(b,c)
      case _ => false
    }

    def isAtLeastAsLarge(x: Observation[_], y: Observation[_]): Boolean = (x,y) match {
      case (Times(a, b, _), Times(c, d, _)) if a == c => isAtLeastAsLarge(b, d)
      case (Plus(a, b, _), Plus(c, d, _)) if a == c => isAtLeastAsLarge(b, d)
      case (Constant(a), Constant(b)) => a.asInstanceOf[Length] >= b.asInstanceOf[Length]
      case _ => false
    }

    {
      case (w@When(a, Require(x) :: _), When(b, Require(y) :: ys))
        if isComplement(a,b) && implies(x,y) =>
        if (ys.nonEmpty)
          List(Require(y), w, When(b, ys))
        else
          List(Require(y), w)
      case (Require(a), When(_, Require(c) :: Nil))
        if implies(a,c) => List(Require(a))
      case (Require(a), Require(b)) if implies(a,b) => List(Require(a))
    }
  }


  /**
   * Compute the amount of lines a tree would occupy when pretty printed.
   * This is used to sort an AST by the size to make sure large subexpressions
   * are close together, increasing their likelihood of being CSE'd.
   */
  def treeSize(stmt: RestrictStatement): Int = stmt match {
    case Require(_) => 1
    case When(_, sub) => 2 + (sub map treeSize).sum
    case RestrictLocation(_, sub) => 2 + (sub map treeSize).sum
  }

  type =?>[A, B] = PartialFunction[A, B]
  /**
   * When given a way to combine two restrict statements, produce a Strategy that traverses an AST
   * to combine as many nodes as possible.
   *
   * This function does most of the heavy lifting.
   */
  def foldChildren(attemptMerge: (RestrictStatement, RestrictStatement) =?> List[RestrictStatement]): Strategy[RestrictStatementF] = {
    /**
     * Given a List of RestrictStatements, fold them, attempting to merge
     * consecutive elements in the list. Since this fold is used as a Strategy,
     * we also track if any merging has actually happened to signal Strategy
     * success or failure.
     *
     * Note that we use Eval here to avoid blowing out our stack on deep ASTs.
     */
    val mergeRules: List[RestrictStatement] => Eval[(Boolean, List[RestrictStatement])] =
      _.foldr(Eval.now((false, List[RestrictStatement]())))((stmt, acc) => acc.map {
        case (anySuccess, Nil) => (anySuccess, List(stmt))
        case (anySuccess, lastMerged :: mergedRules) =>
          (attemptMerge.lift((stmt, lastMerged)) orElse attemptMerge.lift((lastMerged, stmt))).fold(
            (anySuccess, stmt :: lastMerged :: mergedRules)
          ) {
            successfullyMerged => (true, successfullyMerged ++ mergedRules)
          }
      })

    /**
     * Apply [[mergeRules]] to all child expressions of a When or Restrict.
     */
    ruleOption[RestrictStatementF] {
      case RestrictLocation(l, rs) =>
        val (didMergeHappen, rs2) = mergeRules.apply(rs).value
        if (didMergeHappen) Option(RestrictLocation(l, rs2)) else Option.empty
      case When(c, rs) =>
        val (didMergeHappen, rs2) = mergeRules.apply(rs).value
        if (didMergeHappen) Option(When(c, rs2)) else Option.empty
      case _ => Option.empty
    }
  }

  /**
   * Used in common subexpression elimination to determine heuristically how much
   * duplication is allowed before duplicate expressions are extracted.
   */
  def hasEnoughOverlap(xs: List[RestrictStatement], ys: List[RestrictStatement]): Boolean =
    (xs.toSet & ys.toSet).toList.map(treeSize).sum >= 10


  /**
   * Sort an AST by some criteria. This may seem a little weird,
   * but sorting an AST alphabetically increases the likelihood
   * that two neighbouring are similar enough to be merged.
   */
  def sortBy[A: Ordering](f: RestrictStatement => A): Strategy[RestrictStatementF] =
    attempt (rule[RestrictStatementF] {
      case RestrictLocation(l, rs) => RestrictLocation(l, rs.sortBy(f))
      case When(c, rs) => When(c, rs.sortBy(f))
    })
}