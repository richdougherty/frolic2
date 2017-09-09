package nz.rd.frolic.integrations.opentracing

import java.util.concurrent.atomic.AtomicInteger

import io.opentracing.{ActiveSpan, ActiveSpanSource, Span}
import nz.rd.frolic.integrations.opentracing.SuspendableActiveSpanSource.StackContinuation

import scala.annotation.tailrec

final class SuspendableActiveSpanSource extends ActiveSpanSource with ActiveSpanStackSource {

  final private[opentracing] val localStack = new ThreadLocal[List[ActiveSpan]] {
    override def initialValue(): List[SuspendableActiveSpan] = Nil
  }

  override def activeSpan: ActiveSpan = {
    val currentStack: List[ActiveSpan] = localStack.get
    currentStack match {
      case Nil => null // `null` means span not found
      case top :: _ =>
        //assert(top.refCount.get > 0, s"ActiveSpan in thread must have positive refcount: ${top.refCount.get}")
        top
    }

  }

  override def makeActive(span: Span): ActiveSpan = {
    val activeSpan = new SuspendableActiveSpan(this, span, new AtomicInteger(1))
    val currentStack: List[ActiveSpan] = localStack.get
    val newStack: List[ActiveSpan] = activeSpan :: currentStack
    localStack.set(newStack)
    activeSpan
  }

  def deactivateStack(): Unit = {
    @tailrec
    def loop(): Unit = {
      val currentSpan: ActiveSpan = activeSpan
      currentSpan match {
        case null => ()
        case as =>
          as.deactivate() // This will also update the thread-local
          loop()
      }
    }
    loop()
  }

  def captureStack(): StackContinuation = {
    val currentStack: List[ActiveSpan] = localStack.get
    val capturedStack: List[ActiveSpan.Continuation] = currentStack.map(_.capture())
    new StackContinuation(capturedStack)
  }
}

private[opentracing] object SuspendableActiveSpanSource {
  private[opentracing] final class StackContinuation private[opentracing] (
      ks: List[ActiveSpan.Continuation]) extends ActiveStackContinuation {

    // Reactivate spans in current thread.
    override def activate(): Unit = {
      ks.reverse.foreach(_.activate())
    }
  }
}