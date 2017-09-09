package nz.rd.frolic.integrations.opentracing

trait ActiveSpanStackSource {
  def captureStack(): ActiveStackContinuation
}
