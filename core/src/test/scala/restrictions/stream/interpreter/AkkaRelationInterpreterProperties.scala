package restrictions
package stream
package interpreter

import cats.data.Ior
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import restrictions.stream.ast._
import restrictions.relation.ast._

import scala.language.implicitConversions

class AkkaRelationInterpreterProperties extends
  PropSpec with
  GeneratorDrivenPropertyChecks with
  AkkaDSLHelpers with
  Matchers {
  import GenStreamDSL._

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000, workers = 8, minSize = 5, sizeRange = 10)


  /**
    * All RelationDSL terms should terminate when interpreted and evaluated
    */
  property("interpreting and evaluation terminates") {
    forAll { relation: RelationDSL[Key, Int] => eval(relation) }
  }

  property("evaluation always has a unique outcome") {
    forAll { relation: RelationDSL[Key, Int] =>
      relation evaluatesTo relation
    }
  }


  property("map identity") {
    forAll { relation: RelationDSL[Key, Int] =>
      (relation map id) evaluatesTo relation
    }
  }


  property("map composition") {
    forAll { (relation: RelationDSL[Key, Int], f: Int => String, g: String => Double) =>
      (relation map (f andThen g)) evaluatesTo (relation map f map g)
    }
  }

  property("filter identity") {
    forAll { relation: RelationDSL[Key, Int] =>
      relation filter (_ => true) evaluatesTo relation
    }
  }

  property("filter composition") {
    forAll { (relation: RelationDSL[Key, Int], f: Int => Boolean, g: Int => Boolean) =>
      (relation filter f filter g) evaluatesTo (relation filter (x => f(x) && g(x)))
    }
  }

  def reassociateTuple[A,B,C](t: ((A, B), C)): (A, (B, C)) = (t._1._1, (t._1._2, t._2))

  def reassociateIor[A,B,C](ior: Ior[Ior[A,B],C]): Ior[A,Ior[B,C]] = ior match {
    case Ior.Left(Ior.Left(a)) => Ior.left(a)
    case Ior.Left(Ior.Right(b)) => Ior.right(Ior.left(b))
    case Ior.Left(Ior.Both(a, b)) => Ior.both(a, Ior.left(b))
    case Ior.Right(c) => Ior.right(Ior.right(c))
    case Ior.Both(Ior.Left(a), c) => Ior.both(a, Ior.right(c))
    case Ior.Both(Ior.Right(b), c) => Ior.right(Ior.both(b,c))
    case Ior.Both(Ior.Both(a,b), c) => Ior.both(a, Ior.both(b,c))
  }

  /**
    * The order in which we join relations should not influence the result of the join.
    */
  property("inner join is associative") {
    forAll { (r1: RelationDSL[Key, Int], r2: RelationDSL[Key, Int], r3: RelationDSL[Key, Int]) =>
      val left = (r1 join r2) join r3
      val right = r1 join (r2 join r3)
      left map reassociateTuple evaluatesTo right
    }
  }

  /**
    * All elements in the join result come from either the `left` or `right` operand.
    */
  property("inner join precision") {
    forAll { (left: RelationDSL[Key, Int], right: RelationDSL[Key,Int]) =>
      (left join right map fst) evaluatesToSubsetOf left
      (left join right map snd) evaluatesToSubsetOf right
    }
  }

  /**
    * All elements from `left` and `right` that can be joined are in the join result.
    */
  property("inner join recall") {
    forAll { (left: RelationDSL[Key, Int], right: RelationDSL[Key, Int]) =>
      val rowsWithSameKey = for {
        (leftKey, leftVal) <- eval(left).toList
        (rightKey, rightVal) <- eval(right).toList
        if leftKey == rightKey
      } yield (leftKey, (leftVal, rightVal))

      rowsWithSameKey isSubsetOf eval(left join right)
    }
  }

  property("outer join is associative") {
    forAll { (r1: RelationDSL[Key, Int], r2: RelationDSL[Key, Int], r3: RelationDSL[Key, Int]) =>
      val left = ((r1 outerJoin r2) outerJoin r3) map reassociateIor[Int, Int, Int]
      val right = r1 outerJoin (r2 outerJoin r3)
      left evaluatesTo right
    }
  }

  property("outer join precision") {
    forAll { (left: RelationDSL[Key, Int], right: RelationDSL[Key, Int]) =>
      val j = left outerJoin right
      (j filter (_.left.isDefined) map (_.left.get)) evaluatesToSubsetOf left
      (j filter (_.right.isDefined) map (_.right.get)) evaluatesToSubsetOf right
    }
  }

  property("outer join recall") {
    forAll { (left: RelationDSL[Key, Int], right: RelationDSL[Key, Int]) =>
      val j = left outerJoin right
      left evaluatesToSubsetOf (j filter (_.left.isDefined) map (_.left.get))
      right evaluatesToSubsetOf (j filter (_.right.isDefined) map (_.right.get))
    }
  }

  case object Q

  property("zip with stream has a right identity") {
    forAll { relation: RelationDSL[Key, Int] =>
      (relation zip now(Q)) evaluatesTo (relation map ((_,Q)))
    }
  }

  property("relation.changelog.toRelation == relation") {
    forAll { relation: RelationDSL[Key, Int] =>
      relation.changelog.toRelation evaluatesTo relation
    }
  }
}
