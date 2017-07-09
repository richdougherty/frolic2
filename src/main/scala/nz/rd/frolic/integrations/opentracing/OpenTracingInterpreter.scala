package nz.rd.frolic.integrations.opentracing

import io.opentracing.{ActiveSpan, Tracer}
import nz.rd.frolic.async.{Context, Interpreter, InterpreterListener, Task}

final class OpenTracingInterpreterListener(tracer: Tracer) extends InterpreterListener {
  import OpenTracingInterpreterListener._

  override def afterStart(): Unit = {
    val span: ActiveSpan = tracer
        .buildSpan("RunTask")
        .startActive() // Create span and push onto thread [span refcount = 1]
    Context(SpanKey) = span
  }
  override def beforeSuspend(): Unit = {
    val span = Context(SpanKey)
    Context(SpanContinuationKey) = span.capture() // Create async reference to span [span refcount = 2]
    span.deactivate() // Restore previous span in this thread [span refcount = 1]
  }
  override def afterResume(): Unit = {
    Context(SpanContinuationKey).activate() // Push span onto thread [span refcount = 1]
  }
  override def beforeComplete(): Unit = {
    Context(SpanKey).deactivate() // Finish span and pop off thread [span refcount = 0]
  }
}

object OpenTracingInterpreterListener {
  private[OpenTracingInterpreterListener] val SpanKey = new Context.Key[ActiveSpan]("Span")
  private[OpenTracingInterpreterListener] val SpanContinuationKey = new Context.Key[ActiveSpan.Continuation]("SpanContinuation")
}