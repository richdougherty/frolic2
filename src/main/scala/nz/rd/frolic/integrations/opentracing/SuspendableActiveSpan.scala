package nz.rd.frolic.integrations.opentracing

import java.util.concurrent.atomic.AtomicInteger

import io.opentracing.{ActiveSpan, Span, SpanContext}

private[opentracing] class SuspendableActiveSpan (
    val source: SuspendableActiveSpanSource,
    val wrapped: Span,
    val refCount: AtomicInteger) extends ActiveSpan with ActiveSpan.Continuation {

  override def toString: String = wrapped.toString

  override def deactivate(): Unit = {
    val thisSpan = this
    source.localStack.get match {
      case `thisSpan` :: tail => source.localStack.set(tail)
      case stack => throw new IllegalStateException(s"Can't deactivate ActiveSpan $this that isn't on top of stack $stack")
    }
  }

  override def capture = {
    refCount.incrementAndGet()
    this
  }

  override def activate(): ActiveSpan = {
    source.localStack.set(this :: source.localStack.get)
    this
  }

  override def context: SpanContext = wrapped.context

  override def setTag(key: String, value: String): SuspendableActiveSpan = {
    wrapped.setTag(key, value)
    this
  }

  override def setTag(key: String, value: Boolean): SuspendableActiveSpan = {
    wrapped.setTag(key, value)
    this
  }

  override def setTag(key: String, value: Number): SuspendableActiveSpan = {
    wrapped.setTag(key, value)
    this
  }

  override def log(fields: java.util.Map[String, _]): SuspendableActiveSpan = {
    wrapped.log(fields)
    this
  }

  override def log(timestampMicroseconds: Long, fields: java.util.Map[String, _]): SuspendableActiveSpan = {
    wrapped.log(timestampMicroseconds, fields)
    this
  }

  override def log(event: String): SuspendableActiveSpan = {
    wrapped.log(event)
    this
  }

  override def log(timestampMicroseconds: Long, event: String): SuspendableActiveSpan = {
    wrapped.log(timestampMicroseconds, event)
    this
  }

  override def setBaggageItem(key: String, value: String): SuspendableActiveSpan = {
    wrapped.setBaggageItem(key, value)
    this
  }

  override def getBaggageItem(key: String): String = wrapped.getBaggageItem(key)

  override def setOperationName(operationName: String): SuspendableActiveSpan = {
    wrapped.setOperationName(operationName)
    this
  }

  override def log(eventName: String, payload: Any): SuspendableActiveSpan = {
    wrapped.log(eventName, payload)
    this
  }

  override def log(timestampMicroseconds: Long, eventName: String, payload: Any): SuspendableActiveSpan = {
    wrapped.log(timestampMicroseconds, eventName, payload)
    this
  }

  override def close(): Unit = {
    deactivate()
  }

}
