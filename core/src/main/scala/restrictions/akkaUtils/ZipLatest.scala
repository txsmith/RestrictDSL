package restrictions.akkaUtils

import akka.stream.stage.{GraphStage, GraphStageLogic, InHandler, OutHandler}
import akka.stream.{Attributes, FanInShape2, Inlet, Outlet}


class ZipLatest[Left, Right] extends GraphStage[FanInShape2[Left, Right, (Left,Right)]] {
  override val shape: FanInShape2[Left, Right, (Left,Right)] =
    new FanInShape2[Left, Right, (Left, Right)]("ZipLatest")

  val out: Outlet[(Left, Right)] = shape.out
  val inLeft: Inlet[Left] = shape.in0
  val inRight: Inlet[Right] = shape.in1

  override def createLogic(inheritedAttributes: Attributes): GraphStageLogic = new GraphStageLogic(shape) { outer =>

    sealed trait DownstreamState
    case object Available extends DownstreamState
    case object Unavailable extends DownstreamState

    sealed trait IsDirty
    case object Dirty extends IsDirty
    case object Stale extends IsDirty

    sealed trait UpstreamState
    case object Running extends UpstreamState
    case object PartialCompleted extends UpstreamState
    case object Completed extends UpstreamState


    case class State(downstream: DownstreamState,
                     upstream: UpstreamState,
                     inputs: IsDirty,
                     leftValue: Option[Left],
                     rightValue: Option[Right])

    var state: State = State(Unavailable, Running, Stale, None, None)

    val leftInHandler = new InletHandler(inLeft, (s, t: Left) => s.copy(leftValue = Option(t)))
    val rightInHandler = new InletHandler(inRight, (s, t: Right) => s.copy(rightValue = Option(t)))
    val outHandler = new OutletHandler(out)

    override def preStart(): Unit = {
      pull(inLeft)
      pull(inRight)
    }

    setHandler(out, outHandler)
    setHandler(inLeft, leftInHandler)
    setHandler(inRight, rightInHandler)

    private def handleStateChange(currentState: State): State = currentState match {
      case State(Available, upstream, Dirty, Some(l), Some(r)) =>
        push(out, (l, r))
        if (upstream == Completed) completeStage()
        currentState.copy(inputs = Stale, downstream = Unavailable)
      case State(_, Completed, inputState, left, right)
        if inputState == Stale || left.isEmpty || right.isEmpty =>
        completeStage()
        currentState
      case _ =>
        currentState
    }

    private class InletHandler[T](in: Inlet[T], updateElement: (State, T) => State) extends InHandler {
      override def onPush(): Unit = {
        state = handleStateChange(updateElement(state.copy(inputs = Dirty), outer.grab(in)))
        pull(in)
      }

      override def onUpstreamFinish(): Unit = state.upstream match {
        case Running =>
          state = handleStateChange(state.copy(upstream = PartialCompleted))
        case PartialCompleted =>
          state = handleStateChange(state.copy(upstream = Completed))
        case Completed => // Nothing
      }
    }

    private class OutletHandler[T](out: Outlet[T]) extends OutHandler {
      override def onPull(): Unit = {
        state = handleStateChange(state.copy(downstream = Available))
      }
    }
  }
}
