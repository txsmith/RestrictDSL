package restrictions.akkaUtils

import akka.Done
import akka.stream.stage.{GraphStageLogic, GraphStageWithMaterializedValue, InHandler}
import akka.stream.{Attributes, Inlet, SinkShape}
import restrictions.relation.ast._

import scala.concurrent.{ExecutionContext, Future, Promise}

/**
  * Interactively displays the a table in the terminal
  * @param headerLabels - The header labels for each column in the table
  * @param selectAction - An action run when a table row is selected with space or enter
  * @tparam K - The key type for eacht row of the table
  * @tparam V - Type of tuple-values representing rows
  */
class TableViewerSink[K,V <: Product](headerLabels: Seq[String],
                                      selectAction: (K,V) => Unit = (_: K, _: V) => {})
                                     (implicit ec: ExecutionContext)
  extends GraphStageWithMaterializedValue[SinkShape[RowUpdate[K, V]], Future[Done]] {
  val in: Inlet[RowUpdate[K, V]] = Inlet("TableSink")
  override val shape: SinkShape[RowUpdate[K, V]] = SinkShape(in)

  override def createLogicAndMaterializedValue(inheritedAttributes: Attributes): (GraphStageLogic, Future[Done]) = {
    val done: Promise[Done] = Promise[Done]()
    val logic = new GraphStageLogic(shape) {

      val state = scala.collection.mutable.Map[K, V]()

      override def preStart(): Unit = {
        done.completeWith( Future {
            TableUI[K, V](headerLabels, state, selectAction)
            Done
        })
        pull(in)
      }

      setHandler(in, new InHandler {
        override def onPush(): Unit = {
          grab(in) match {
            case Update(key, value) =>
              state.get(key) match {
                case None => state.update(key, value)
                case Some(presentValue) if presentValue != value => state.update(key, value)
                case _ =>
              }
            case Delete(key) =>
              state.get(key) match {
                case None =>
                case Some(_) => state.remove(key)
              }
          }
          pull(in)
        }
      })
    }
    (logic, done.future)
  }
}

