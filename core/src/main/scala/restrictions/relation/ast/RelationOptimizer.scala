package restrictions.relation.ast

import restrictions._
import restrictions.stream.{ast => stream}
import restrictions.stream.ast.StreamOptimizer
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

object RelationOptimizer {

  def optimiseStrategy[K,A]: Strategy = {
    // Everything operating on Never() can be eliminated
    val eliminateNoOps = rule[RelationDSL[K,A]] {
      case Map(Never(), _) => Never()
      case Filter(Never(), _) => Never()
      case InnerJoin(Never(), _) => Never()
      case InnerJoin(_, Never()) => Never()
      case Zip(Never(), _) => Never()
      case Zip(_, stream.Never()) => Never()
    }

    // Consecutive operations of the same kind can be fused.
    val fuseConsecutive = rule[RelationDSL[K,A]] {
      case Map(Map(t, f), g) => Map(t, g.compose(f).asInstanceOf[Any => A])
      case Filter(Filter(xs, f), g) => Filter(xs, (x: A) => g(x) && f(x))
    }

    // r zip now(x) == r map (_,x)
    val eliminateConstantZip = rule[RelationDSL[K,A]] {
      case Zip(r, stream.Now(a)) =>
        val f = ((x: Any) => (x, a)).name(s"{x => (x,$a)}")
        Map(r, f)
    }

    bottomup (attempt (eliminateNoOps + fuseConsecutive + eliminateConstantZip)) <* StreamOptimizer.optimiseStrategy
  }

  def optimise[K,A](term: RelationDSL[K,A]): RelationDSL[K,A] = {
    optimiseStrategy(term).get.asInstanceOf[RelationDSL[K,A]]
  }
}
