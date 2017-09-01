package nz.rd.frolic.integrations.opentracing

import io.opentracing.{ActiveSpan, References, Span, Tracer}
import nz.rd.frolic.async.{Context, InterpreterListener}
import nz.rd.frolic.integrations.opentracing.OpenTracingInterpreterListener.SuspendedKey
import nz.rd.frolic.integrations.opentracing.SuspendableActiveSpanSource.StackContinuation

import scala.annotation.tailrec

final class OpenTracingInterpreterListener(tracer: SuspendableTracer) extends InterpreterListener {

  import OpenTracingInterpreterListener._

  override type ActiveData = OrigLocalStack
  override type SuspendingData = OrigLocalStack
  override type ResumingData = OrigLocalStackAndTaskStack

  private def activeSpanSource: SuspendableActiveSpanSource = tracer.suspendableActiveSpanSource

  override def starting(): OrigLocalStack = {
    println("<starting>")

    val taskSpan: Span = tracer.buildSpan("task").startManual()

    // Capture the original local stack then detach it from this thread
    val localStackK: StackContinuation = activeSpanSource.captureStack()
    activeSpanSource.deactivateStack()

    // Active task span in its new empty stack
    tracer.makeActive(taskSpan)

    OrigLocalStack(localStackK)
  }

  override def suspending(origLocalStack: OrigLocalStack): OrigLocalStack = {
    println("<suspending>")

    val suspendSpan: Span = tracer.buildSpan("suspend").startManual()

    // Create span with 'suspend' as parent, but don't attach
    val suspendingSpan: Span = tracer.buildSpan("suspending").asChildOf(suspendSpan).startManual()

    // Suspend the task stack and remove it from the local thread
    val taskStackK: StackContinuation = activeSpanSource.captureStack()

    // Make the current stack empty
    activeSpanSource.deactivateStack()

    // Save the suspend info for when we resume
    Context(SuspendedKey) = TracingSuspendedContextData(taskStackK, suspendSpan)

    // Now we have an empty stack with just the 'suspending' span on it
    tracer.makeActive(suspendingSpan)

    origLocalStack
  }

  override def suspended(origLocalStack: OrigLocalStack): Unit = {
    println("<suspended>")

    activeSpanSource.deactivateStack()
    origLocalStack.origLocalStack.activate()
  }

  override def resuming(): OrigLocalStackAndTaskStack = {
    println("<resuming>")

    val suspendedInfo: TracingSuspendedContextData = Context(SuspendedKey)
    Context(SuspendedKey) = null // Clear so we can't accidentally use the value twice

    // Create span with 'suspend' as parent, but don't attach
    val resumingSpan: Span = tracer.buildSpan("resuming").asChildOf(suspendedInfo.suspendSpan).startManual()

    // Capture the original local stack then detach it from this thread
    val localStackK: StackContinuation = activeSpanSource.captureStack()
    activeSpanSource.deactivateStack()

    // Active resuming span in its new empty stack
    tracer.makeActive(resumingSpan)

    // Declare ths suspend span as finished (although resuming might still take some time)
    suspendedInfo.suspendSpan.finish()

    OrigLocalStackAndTaskStack(localStackK, suspendedInfo.taskStackK)
  }

  override def resumed(origLocalStackAndTaskStack: OrigLocalStackAndTaskStack): OrigLocalStack = {
    println("<resumed>")

    // Clear resuming stack
    activeSpanSource.deactivateStack()

    // Activate task stack
    origLocalStackAndTaskStack.taskStack.activate()

    OrigLocalStack(origLocalStackAndTaskStack.origLocalStack)
  }

  override def completing(origLocalStack: OrigLocalStack): Unit = {
    println("<completing>")

    // Clear task stack
    activeSpanSource.deactivateStack()

    // Reactivate original thread local stack
    origLocalStack.origLocalStack.activate()
  }
}

private[opentracing] object OpenTracingInterpreterListener {

  case class TracingSuspended(
      taskStackK: StackContinuation
  )

  case class OrigLocalStack(
      origLocalStack: StackContinuation
  )

  case class OrigLocalStackAndTaskStack(
      origLocalStack: StackContinuation,
      taskStack: StackContinuation
  )

  case class TracingSuspendedContextData(
      taskStackK: StackContinuation,
      suspendSpan: Span
  )
//
//  case class TracingActiveData(
//      localStackK: StackContinuation,
//      taskSpan: ActiveSpan
//  )
//
//  case class SuspendedContextData(
//      taskStackK: StackContinuation,
//      runSpan: ActiveSpan,
//      suspendSpan: ActiveSpan
//  )
//
//  case class TracingSuspendingData(
//      operationSpan: ActiveSpan, // either start or resume
//      runSpan: ActiveSpan
//  )
//
//  case class Suspend(
//      origSpanContinuation: ActiveSpan.Continuation,
//      suspendSpanContinuation: ActiveSpan.Continuation
//  )
//  case class Resuming(
//      runSpanContinuation: ActiveSpan.Continuation,
//      suspendSpan: ActiveSpan
//  )

  val SuspendedKey = new Context.Key[TracingSuspendedContextData]("OpenTracingSuspended")
}