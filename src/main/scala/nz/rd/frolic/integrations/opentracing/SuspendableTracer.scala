package nz.rd.frolic.integrations.opentracing

import io.opentracing.propagation.Format
import io.opentracing.{ActiveSpan, Span, SpanContext, Tracer}

class SuspendableTracer(
    wrapped: Tracer,
    val suspendableActiveSpanSource: SuspendableActiveSpanSource) extends Tracer {

  override def buildSpan(operationName: String): Tracer.SpanBuilder =
    wrapped.buildSpan(operationName)
  override def extract[C](format: Format[C], carrier: C): SpanContext =
    wrapped.extract(format, carrier)
  override def inject[C](spanContext: SpanContext, format: Format[C], carrier: C): Unit =
    wrapped.inject(spanContext, format, carrier)
  override def activeSpan(): ActiveSpan =
    wrapped.activeSpan()
  override def makeActive(span: Span): ActiveSpan =
    wrapped.makeActive(span)
}
