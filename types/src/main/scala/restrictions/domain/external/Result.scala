package restrictions.domain.external

import cats.Semigroup

sealed trait Result {
  val isAllow: Boolean
}
case object Allow extends Result {
  override val isAllow: Boolean = true
}
case class Deny(violatedCondition: String) extends Result {
  override val isAllow: Boolean = false
}

object Result {
  def isAllow(r: Result): Boolean = r.isAllow
  def apply(t: (Boolean, String)): Result = if (t._1) {
    Allow
  } else {
    Deny(t._2)
  }

  implicit val ResultSemigroup: Semigroup[Result] = new ResultConjunctionSemigroup

  class ResultConjunctionSemigroup extends Semigroup[Result] {
    override def combine(x: Result, y: Result): Result = x match {
      case Allow => y
      case Deny(_) => x
    }
  }
}
