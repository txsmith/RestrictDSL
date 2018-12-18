import cats.implicits._
import cats.free.Free
import restrictions.domain._
import restrictions.domain.external._
import restrictions._
import restrictions.ast.{Observation => _, _}
import higherkindness.droste.data.Fix

import scala.language.higherKinds


/**
 * Transform individual rows of a CSV export from Portmaps into a RestrictStatement AST.
 *
 * Required knowledge:
 *   - Functor
 *   - Free
 */
object RuleParser {

  /**
   * Here is a representation of a single row of a restriction table from Portmaps.
   * See `waalhaven.csv` in the resources folder for the real thing.
   *
   * Our objective is to turn this row into a value of type `RestrictStatement`.
   * We're going to build this value incrementally, in small steps, making the
   * RestrictStatement tree deeper each time.
   *
   * Note that each field in this case class can be empty,
   * since we're not sure whether any field is present in each row.
   *
   * After turning each individual row into a separate RestrictStatement, we make an
   * effort to combine them in Main.
   */
  case class UnknownRule(FEATUREID: Option[BerthIdentifier],
                          RESTRICTION_TYPE: Option[RestrictionType],
                          VESSEL_TYPE: Option[String],
                          VESSEL_LENGTH_FROM: Option[Double],
                          VESSEL_LENGTH_TO: Option[Double],
                          VESSEL_BEAM_FROM: Option[Double],
                          VESSEL_BEAM_TO: Option[Double],
                          VESSEL_DRAUGHT_FROM: Option[Double],
                          VESSEL_DRAUGHT_TO: Option[Double],
                          VESSEL_HEIGHT_FROM: Option[Double],
                          VESSEL_HEIGHT_TO: Option[Double],
                          VESSEL_TONNAGE_FROM: Option[Double],
                          VESSEL_TONNAGE_TO: Option[Double],
                          VESSEL_DIRECTION: Option[String],
                          SPECIAL_TRANSPORT: Option[String],
                          VISIBILITY_FROM: Option[Double],
                          VISIBILITY_TO: Option[Double],
                          WIND_DIRECTION_FROM: Option[String],
                          WIND_DIRECTION_TO: Option[String],
                          WIND_SPEED_FROM: Option[Double],
                          WIND_SPEED_TO: Option[Double],
                          CURRENT_FROM: Option[String],
                          CURRENT_TO: Option[String],
                          CURRENT_TREND: Option[String],
                          LIGHT_CONDITION: Option[String],
                          RESTRICTION_VALUE: Option[String],
                          RESTRICTION_UNIT: Option[String],
                          RESTRICTION_DESCRIPTION: Option[String],
                          POLICY_CLEARANCE: Option[String],
                          POLICY_TUG_NUMBER: Option[String],
                          POLICY_TUG_TYPE: Option[String],
                          POLICY_TUG_BOLLARD_PULL: Option[String],
                          POLICY_REMARKS: Option[String],
                          STATUS: Option[String],
                          DATASOURCE: Option[String],
                          SURVEYMETHOD: Option[String],
                          SURVEYPRECISION: Option[String],
                          SURVEYDATE: Option[String],
                          FUNCSTARTDATE: Option[String],
                          FUNCENDDATE: Option[String])

  sealed trait RestrictionType
  case object MINIMUM_UKC extends RestrictionType
  case object MINIMUM_UKC_LONG_TERM extends RestrictionType
  case object FRESH_WATER_ALLOWANCE extends RestrictionType
  case object VESSEL_TYPE extends RestrictionType
  case object MAXIMUM_BEAM extends RestrictionType
  case object MINIMUM_LENGTH extends RestrictionType
  case object MAXIMUM_LENGTH extends RestrictionType

  import ParseRules._

  /**
   * To parse the above monster case class into an AST,
   * we perform these steps:
   */
  val parseRule: RuleParser =
    parseLocation andThen
    parseVesselTypes andThen
    parseDirection andThen
    (parseDraughtFrom or parseDraughtTo) andThen (
      parseVesselTypeRestriction or
      parseMinUKC or
      parseFreshWaterAllowance or
      parseMaxBeam or
      parseMaxLength or
      parseMinLength
    )

  /**
   * All the above steps are [[RuleParser]]s.
   *
   * A rule parser is a function.
   * It takes one UnknownRule as argument, and produces a Free[RestrictStatementF, UnknownRule] as result.
   * Free allows us to build an AST incrementally, deepening the tree one step at a time.
   * Note that this representation does not have a notion of failure. Parsing never results in an exception or error.
   * If some RuleParser fails to recognize an UnknownRule, it will simply not expand the AST.
   */
  trait RuleParser extends (UnknownRule => Free[RestrictStatementF, UnknownRule]) {
    /**
     * Perform one step after the other.
     * Will always evaluate both the left as well as the right-hand side.
     */
    def andThen(g: RuleParser): RuleParser = this(_) flatMap g

    /**
     * Parse either this step or the other.
     * If the left succeeds, the right-hand side will not be evaluated.
     * Used for mutually exclusive choice.
     */
    def or(g: RuleParser): RuleParser = unparsedRule => {
      val leftResult = this(unparsedRule)
      leftResult.fold(_ => g(unparsedRule), _ => leftResult)
    }
  }

  def name[T](n: String) = Named(n, Void[T]())


  /**
   * Once we're done with all the parse steps, and we know for sure that there are no unparsed holes left in the AST,
   * we can turn it into an AST without holes by getting rid of the Free structure and replacing it with Fix.
   */
  def toTotalAST(tree: Free[RestrictStatementF, _]): RestrictStatement =
    tree.fold(
      a => throw new RuntimeException(s"AST is incomplete. Found leaf: $a"),
      rule => Fix(rule.map(toTotalAST(_))))

  /**
   * Each individual parse step takes an [[UnknownRule]] and turns it into one level of [[RestrictStatementF]].
   */
  object ParseRules {

    val parseLocation: RuleParser = r => r.FEATUREID match {
      case None => Free.pure(r)
      case Some(loc) => Free.liftF(RestrictLocationF(loc, List(r)))
    }

    val parseVesselTypes: RuleParser = r => r.VESSEL_TYPE match {
      case None => Free.pure(r)
      case Some("ALL") => Free.pure(r)
      case Some(t) => Free.liftF(WhenF(name[VesselType]("vessel.type") === Read[VesselType].reads(t).get, List(r)))
    }

    val parseDirection: RuleParser = r => r.VESSEL_DIRECTION match {
      case Some("INBOUND_OUTBOUND") => Free.liftF(
        WhenF(name[MovementType]("vessel.direction") === Inbound || name[MovementType]("vessel.direction") === Outbound, List(r))
      )
      case _ => Free.pure(r)
    }

    val parseVesselTypeRestriction: RuleParser = r => (r.RESTRICTION_TYPE, r.RESTRICTION_VALUE) match {
      case (Some(VESSEL_TYPE), Some("0")) => Free.liftF(RequireF(false))
      case (Some(VESSEL_TYPE), Some("1")) => Free.liftF(RequireF(true))
      case _ => Free.pure(r)
    }

    val parseDraughtFrom: RuleParser = r => r.VESSEL_DRAUGHT_FROM match {
      case Some(draught) => Free.liftF(WhenF(name[Length]("vessel.draught") >= draught.meters, List(r)))
      case _ => Free.pure(r)
    }

    val parseDraughtTo: RuleParser = r => r.VESSEL_DRAUGHT_TO match {
      case Some(draught) => Free.liftF(WhenF(name[Length]("vessel.draught") < draught.meters, List(r)))
      case _ => Free.pure(r)
    }

    val parseMinUKC: RuleParser = r => (r.RESTRICTION_TYPE, r.RESTRICTION_VALUE, r.RESTRICTION_UNIT) match {
      case (Some(rType), Some(ukcStr), Some("METERS")) if rType == MINIMUM_UKC || rType == MINIMUM_UKC_LONG_TERM =>
        val ukc = Read[Double].reads(ukcStr).get.meters
        Free.liftF(RequireF(name[Length]("contract_depth") >= name[Length]("vessel.draught") + constant(ukc)))
      case _ => Free.pure(r)
    }

    val parseFreshWaterAllowance: RuleParser = r => (r.RESTRICTION_TYPE, r.RESTRICTION_VALUE, r.RESTRICTION_UNIT) match {
      case (Some(FRESH_WATER_ALLOWANCE), Some(percentageStr), Some("PERCENTAGE")) =>
        val percentage = Read[Double].reads(percentageStr).get
        Free.liftF(RequireF(name[Double]("fresh_water_allowance") === percentage))
      case _ => Free.pure(r)
    }

    val parseMaxBeam: RuleParser = r => (r.RESTRICTION_TYPE, r.RESTRICTION_VALUE, r.RESTRICTION_UNIT) match {
      case (Some(MAXIMUM_BEAM), Some(beamStr), Some("METERS")) =>
        val beam = Read[Double].reads(beamStr).get.meters
        Free.liftF(RequireF(name[Length](s"vessel.beam") <= beam))
      case _ => Free.pure(r)
    }

    val parseMaxLength: RuleParser = r => (r.RESTRICTION_TYPE, r.RESTRICTION_VALUE, r.RESTRICTION_UNIT) match {
      case (Some(MAXIMUM_LENGTH), Some(lengthStr), Some("METERS")) =>
        val length = Read[Double].reads(lengthStr).get.meters
        Free.liftF(RequireF(name[Length](s"vessel.length") <= length))
      case _ => Free.pure(r)
    }

    val parseMinLength: RuleParser = r => (r.RESTRICTION_TYPE, r.RESTRICTION_VALUE, r.RESTRICTION_UNIT) match {
      case (Some(MINIMUM_LENGTH), Some(lengthStr), Some("METERS")) =>
        val length = Read[Double].reads(lengthStr).get.meters
        Free.liftF(RequireF(name[Length](s"vessel.length") > length))
      case _ => Free.pure(r)
    }
  }
}
