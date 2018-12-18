package restrictions.compilers

import akka.stream.scaladsl.{Merge => _, _}
import akka.NotUsed
import akka.stream.Materializer
import cats.Semigroup
import cats.data.Ior
import cats.implicits._
import restrictions.relation.ast.{Delete, RelationDSL, RowUpdate, Update}
import restrictions.relation.{ast => rel}
import restrictions.stream.ast.StreamDSL
import restrictions.stream.{ast => stream}
import restrictions.akkaUtils._

import scala.collection.mutable
import scala.concurrent.Future

object StreamAkkaCompiler {

  implicit class AkkaTableOps[K,A](source: UpdateSource[K,A]) {
    def toTable(implicit materializer: Materializer): Future[Map[K, A]] =
      source.runFold(Map.empty[K,A])((table: Map[K,A], rowUpdate) => rowUpdate match {
        case Delete(key) => table - key
        case Update(key, value) => table + ((key, value))
      })
  }

  // Some silly aliases to make the types below more clear
  type Key = Any
  type ForeignKey = Any
  type ValueL = Any
  type ValueR = Any

  def compile[A](streamDSL: StreamDSL[A]): Source[A, NotUsed] =
    streamDSL match {
      case stream.FromAkka(akka) => akka
      case stream.FromSeq(seq) => Source(seq)
      case stream.Never() => Source.empty
      case stream.Now(value) => Source.single(value)
      case stream.Named(_, s) => compile(s)
      case stream.Zip(left, right) => compile(left) amazingZipLatest compile(right)
      case stream.Merge(left, right) => compile(left).merge(compile(right))
      case stream.Map(a, f) => compile(a).map(f)
      case stream.MapConcat(a, f) => compile(a).mapConcat(f)
      case stream.Filter(a, p) => compile(a).filter(p)
      case stream.Lookup(stream, relation) =>

        val m: Source[Either[RowUpdate[Key, ValueL], (Key, ValueR)], NotUsed] =
          compile(relation).map(Left(_)) merge
          compile(stream).map(Right(_))

        m.statefulMapConcat { () =>
          val table: mutable.Map[Key, ValueL] = mutable.Map.empty

          {
            case Left(Delete(k)) =>
              table.remove(k)
              List()
            case Left(Update(k,v)) =>
              table.update(k, v)
              List()
            case Right((k,v)) =>
              if (table.contains(k)) {
                List((k, table(k), v))
              } else {
                List()
              }
          }
        }
      case stream.Changelog(relation) => compile(relation)
    }

  def compile[Key,A](relationDSL: RelationDSL[Key,A]): UpdateSource[Key, A] =
    relationDSL match {
      // No materialization of the table happens for ToRelation and Map
      case rel.ToRelation(stream) => compile(stream)

      case rel.Map(r, f) => compile(r).map(_.map(f))

      // Filter and all Joins below are stateful
      case rel.Filter(r, p) =>
        compile(r).statefulMapConcat { () =>
          val table: mutable.Map[Key, A] = mutable.Map.empty

        {
          case Delete(k) =>
            if (table.contains(k)) {
              table.remove(k)
              List(Delete(k))
            } else {
              List()
            }
          case Update(k, v) =>
            if (p(v)) {
              table.update(k, v)
              List(Update(k, v))
            } else if (table.contains(k)) {
              table.remove(k)
              List(Delete(k))
            } else {
              List()
            }
        }
        }

      case rel.InnerJoin(left, right) =>
        val m: Source[Either[RowUpdate[Key, ValueL], RowUpdate[Key, ValueR]], NotUsed] =
          compile(left).map(Left(_)) merge
            compile(right).map(Right(_))

        m.statefulMapConcat { () =>
          val leftTable: mutable.Map[Key, ValueL] = mutable.Map.empty
          val rightTable: mutable.Map[Key, ValueR] = mutable.Map.empty

        {
          case Left(Delete(k)) =>
            leftTable.remove(k)
            if (rightTable.contains(k)) {
              List(Delete(k))
            } else {
              List()
            }
          case Right(Delete(k)) =>
            rightTable.remove(k)
            if (leftTable.contains(k)) {
              List(Delete(k))
            } else {
              List()
            }
          case Left(Update(k, leftVal)) =>
            leftTable.update(k, leftVal)
            if (rightTable.contains(k)) {
              List(Update(k, (leftVal, rightTable(k))))
            } else {
              List()
            }
          case Right(Update(k, rightVal)) =>
            rightTable.update(k, rightVal)
            if (leftTable.contains(k)) {
              List(Update(k, (leftTable(k), rightVal)))
            } else {
              List()
            }
        }
        }

      case rel.OuterJoin(left, right) =>
        val m: Source[Either[RowUpdate[Key, ValueL], RowUpdate[Key, ValueR]], NotUsed] =
          compile(left).map(Left(_)) merge
            compile(right).map(Right(_))
        m.statefulMapConcat { () =>
          val leftTable: mutable.Map[Key, ValueL] = mutable.Map.empty
          val rightTable: mutable.Map[Key, ValueR] = mutable.Map.empty

        {
          case Left(Delete(k)) =>
            leftTable.remove(k)
            if (rightTable.contains(k)) {
              List(Update(k, Ior.right(rightTable(k))))
            } else {
              List(Delete(k))
            }
          case Right(Delete(k)) =>
            rightTable.remove(k)
            if (leftTable.contains(k)) {
              List(Update(k, Ior.left(leftTable(k))))
            } else {
              List(Delete(k))
            }
          case Left(Update(k, leftVal)) =>
            leftTable.update(k, leftVal)
            if (rightTable.contains(k)) {
              List(Update(k, Ior.both(leftVal, rightTable(k))))
            } else {
              List(Update(k, Ior.left(leftVal)))
            }
          case Right(Update(k, rightVal)) =>
            rightTable.update(k, rightVal)
            if (leftTable.contains(k)) {
              List(Update(k, Ior.both(leftTable(k), rightVal)))
            } else {
              List(Update(k, Ior.right(rightVal)))
            }
        }
        }

      case rel.Zip(relation, stream) =>
        val m: Source[Either[RowUpdate[Key, ValueL], ValueR], NotUsed] =
          compile(relation).map(Left(_)) merge
            compile(stream).map(Right(_))

        m.statefulMapConcat { () =>
          val table: mutable.Map[Key, ValueL] = mutable.Map.empty
          var lastStreamElem: Option[ValueR] = Option.empty

          {
            case Left(Delete(k)) =>
              if (table.remove(k).isDefined && lastStreamElem.isDefined) {
                List(Delete(k))
              } else {
                List()
              }
            case Left(Update(k, v)) =>
              table.update(k, v)
              if (lastStreamElem.isDefined) {
                List(Update(k, (v, lastStreamElem.get)))
              } else {
                List()
              }
            case Right(v) =>
              lastStreamElem = Option(v)
              table.toList map { kv => Update(kv._1, (kv._2, v)) }
          }
        }

      case rel.JoinValue(left, right) =>
        val m: Source[Either[RowUpdate[Key, ForeignKey], RowUpdate[ForeignKey, A]], NotUsed] =
          compile(left).map(Left(_)) merge
            compile(right).map(Right(_))

        m.statefulMapConcat { () =>
          val leftTable: mutable.Map[Key, ForeignKey] = mutable.Map.empty
          val rightTable: mutable.Map[ForeignKey, A] = mutable.Map.empty

        /**
          * Performance of this operator can probably be improved by avoiding
          * a scan over the entire left table if the right table changes.
          */
        {
          case Left(Delete(k)) =>
            leftTable.remove(k) match {
              case Some(foreignKey) if rightTable.contains(foreignKey) => List(Delete(k))
              case _ => List()
            }
          case Right(Delete(foreignKey)) =>
            rightTable.remove(foreignKey)
            leftTable.toList filter { _._2 == foreignKey } map { entry => RowUpdate.delete(entry._1) }

          case Left(Update(k, foreignKey)) =>
            leftTable.update(k, foreignKey)
            rightTable.get(foreignKey) match {
              case Some(rightValue) => List(RowUpdate(k, rightValue))
              case _ => List()
            }
          case Right(Update(rightKey, rightVal)) =>
            rightTable.update(rightKey, rightVal)
            leftTable.toList filter { _._2 == rightKey } map { entry => RowUpdate(entry._1, rightVal) }
        }
      }
    }

}
