package nz.rd.frolic.async

trait Continuation[-A] {
  def resume(value: A): Unit
  def resumeWithException(cause: Throwable): Unit
}
