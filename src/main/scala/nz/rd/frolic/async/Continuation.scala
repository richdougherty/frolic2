package nz.rd.frolic.async

/**
 * A minimal continuation class, directly copied from Kotlin's
 * coroutine implementation.
 */
trait Continuation[-A] {
  def resume(value: A): Unit
  def resumeWithException(cause: Throwable): Unit
}
