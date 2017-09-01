package nz.rd.frolic.async

/**
 * A minimal continuation class, directly copied from Kotlin's
 * coroutine implementation.
 */
trait Continuation[-A] {
  def resume(value: A): Unit = resumeWithThunk(Task.Success(value))
  def resumeWithException(cause: Throwable): Unit = resumeWithThunk(Task.Failure(cause))
  def resumeWithThunk(thunk: => Task.Completion[A]): Unit
}
