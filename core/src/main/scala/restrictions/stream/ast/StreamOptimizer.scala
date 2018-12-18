package restrictions.stream.ast

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy

import restrictions._

object StreamOptimizer {

  def optimiseStrategy[A]: Strategy = {
    // Merge distributes over other operations
    val pushdownMerge = rule[StreamDSL[A]] {
      case Merge(Map(xs, f), Map(ys, g)) if f == g => Map(Merge(xs, ys), f)
      case Merge(Filter(xs, p), Filter(ys, q)) if p == q => Filter(Merge(xs, ys), p)
      // case Merge(Join(xs1, ys1), Join(xs2, ys2)) => Join(Merge(xs1, xs2), Merge(ys1, ys2))
    }

    val pullupMerge = rule[StreamDSL[A]] {
      case Filter(Merge(xs, ys), p) => Merge(Filter(xs, p), Filter(ys, p))
      case Map(Merge(xs, ys), f) => Merge(Map(xs, f), Map(ys, f))
      // case Join(Merge(xs1, xs2), Merge(ys1, ys2)) => Merge(Join(xs1, ys1), Join(xs2, ys2))
    }

    // Consecutive operations of the same kind can be fused.
    val fuseConsecutive = rule[StreamDSL[A]] {
      case Map(Map(t, f), g) => Map(t, f.compose(g).asInstanceOf[Any => A])
      case Filter(Filter(xs, f), g) => Filter(xs, (x: A) => g(x) && f(x))
    }

    // Operations on constants can be evaluated directly
    val evaluateConstants = rule[StreamDSL[A]] {
      case Map(Now(a), f) => Now(f(a))
      case Filter(Now(a), f) => if (f(a)) Now(a) else Never()
      case Zip(Now(x), Now(y)) => Now((x,y))
    }

    // Everything operating on Never() can be eliminated
    val eliminateNoOps = rule[StreamDSL[A]] {
      case Merge(Never(), xs) => xs
      case Merge(xs, Never()) => xs
      case Zip(Never(), _) => Never()
      case Zip(_, Never()) => Never()
      case Map(Never(), _) => Never()
      case Filter(Never(), _) => Never()
      case Named(_, Never()) => Never()
    }

    // l zip now(x) == l map (_,x)
    val eliminateConstantJoin = rule[StreamDSL[A]] {
      case Zip(l, Now(a)) =>
        val f = ((x: Any) => (x, a)).name(s"{x => (x,$a)}")
        Map(l, f)
      case Zip(Now(a), r) =>
        val f = ((x: Any) => (x, a)).name(s"{x => ($a,x)}")
        Map(r, f)
    }
    // First move all merge operations as close to the root as possible,
    // this leaves all operations that can be constant-folded at the bottom of the AST,
    // creating more opportunities for rewriting.
    topdown (attempt (pullupMerge)) <*
    // Now perform all fusing and evaluation and finally push merge operations down again
    // to avoid duplicate operations.
    bottomup (repeat (eliminateNoOps + eliminateConstantJoin + evaluateConstants + fuseConsecutive + pushdownMerge))
  }

  def optimise[A](term: StreamDSL[A]): StreamDSL[A] = {
    optimiseStrategy(term).get.asInstanceOf[StreamDSL[A]]
  }
}
