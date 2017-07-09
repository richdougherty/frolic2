package nz.rd.frolic.integrations.opentracing

import io.opentracing.{ActiveSpan, Span, Tracer}
import nz.rd.frolic.async.{Context, InterpreterListener}

final class OpenTracingInterpreterListener(tracer: Tracer) extends InterpreterListener {
  import OpenTracingInterpreterListener._

  override def onStart(): Unit = {
    val state = new State
    // Create run span and push onto thread [refcount = 1]
    state.runSpan = tracer
        .buildSpan("RunTask")
        .startActive()
    Context(StateKey) = state
  }
  override def onSuspend(): Unit = {
    val state = Context(StateKey)

    // Create async reference to run span and remove from this thread [refcount = 1]
    state.runSpanContinuation = state.runSpan.capture()
    state.runSpan.deactivate()

    // Start a span to track the suspending activity [refcount = 1]
    //state.suspendingSpan = tracer.buildSpan("Suspending").startActive()

    // Start a span to track the suspend time [no refcount]
    state.suspendSpan = tracer.buildSpan("Suspend").startManual()
  }
  override def onResume(): Unit = {
    val state = Context(StateKey)

    // The suspend has finished
    state.suspendSpan.finish()
    state.suspendSpan = null
  }
  override def onComplete(): Unit = {
    val state = Context(StateKey)

    // Finish span and pop off thread [span refcount = 0]
    state.runSpan.deactivate()
  }
}

private[opentracing] object OpenTracingInterpreterListener {
  class State {
    var runSpan: ActiveSpan = null
    var runSpanContinuation: ActiveSpan.Continuation = null
    var suspendSpan: Span = null
  }

  val StateKey = new Context.Key[State]("OpenTracingState")
}