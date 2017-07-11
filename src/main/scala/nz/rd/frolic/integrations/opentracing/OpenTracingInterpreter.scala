package nz.rd.frolic.integrations.opentracing

import io.opentracing.{ActiveSpan, Span, Tracer}
import nz.rd.frolic.async.{Context, InterpreterListener}

final class OpenTracingInterpreterListener(tracer: Tracer) extends InterpreterListener {

  import OpenTracingInterpreterListener._

  override type ActiveData = Active
  override type SuspendingData = Suspending

  override def starting(): Active = {
    val runSpan: ActiveSpan = tracer
        .buildSpan("task_run")
        .startActive()
    Active(runSpan)
  }
  override def suspending(activeData: Active): Suspending = {
    // Build child spans of the run span
    val suspendingSpan: ActiveSpan = tracer
        .buildSpan("task_suspending")
        .startActive() // Activate this until suspended() is called
    val suspendedSpan: Span = tracer
        .buildSpan("task_suspended")
        .startManual() // Manual because we're not activating this yet

    // Capture the run span and stop it in this thread
    val runContinuation: ActiveSpan.Continuation = activeData.runSpan.capture()
    activeData.runSpan.deactivate()

    Context(SuspendedKey) = Suspended(runContinuation, suspendedSpan)
    Suspending(suspendingSpan)
  }

  override def suspended(suspendingData: Suspending): Unit = {
    suspendingData.suspendingSpan.deactivate()
  }

  override def resuming(): ActiveData = {
    val suspendedData: Suspended = Context(SuspendedKey)
    val runSpan: ActiveSpan = suspendedData.runSpanContinuation.activate()
    suspendedData.suspendedSpan.finish()
    Context(SuspendedKey) = null // Clear so we can't accidentally use the value twice
    Active(runSpan)
  }
  override def completing(activeData: Active): Unit = {
    activeData.runSpan.deactivate()
  }
}

private[opentracing] object OpenTracingInterpreterListener {
  case class Active(
      runSpan: ActiveSpan
  )
  case class Suspending(
      suspendingSpan: ActiveSpan
  )
  case class Suspended(
      runSpanContinuation: ActiveSpan.Continuation,
      suspendedSpan: Span
  )

  val SuspendedKey = new Context.Key[Suspended]("OpenTracingSuspended")
}