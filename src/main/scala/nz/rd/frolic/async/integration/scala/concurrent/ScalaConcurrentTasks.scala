package nz.rd.frolic.async.integration.scala.concurrent

import nz.rd.frolic.async.{Continuation, Interpreter, Task, Transform}
import nz.rd.frolic.async.Task.{Completion, Failure, Success, Suspend}

import scala.concurrent.{ExecutionContext, Future, Promise}

final object ScalaConcurrentTasks {

  private object SameThreadExecutionContext extends ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
  }

  /**
   * A task which suspends itself then resumes on the Executor thread.
   */
  def execute(ec: ExecutionContext): Suspend[Unit] = {
    Task.suspend { (resume: Continuation[Unit]) => ec.execute { () => resume.resume(()) } }
  }

  def futureOnCompleteTask[A](future: Future[A]): Task[A] = {
    Task.Flatten(new Task.Eval[Task[A]] {
      override def apply(): Task[A] = Task.suspend { (resume: Continuation[A]) =>
        future.onComplete {
          case scala.util.Success(v) => resume.resume(v)
          case scala.util.Failure(t) => resume.resumeWithException(t)
        }(SameThreadExecutionContext)
      }
      override def completion: Option[Completion[Task[A]]] = {
        future.value match {
          case None => None
          case Some(scala.util.Success(v)) => Some(Success(Success(v)))
          case Some(scala.util.Failure(t)) => Some(Success(Failure(t)))
        }
      }
    })
  }

  def interpretFuture[A](task: Task[A], interpreter: Interpreter): Future[A] = {
    val promise = Promise[A]
    interpreter.run(promiseCompleteTask(task, promise))
    promise.future
  }

  private def promiseCompleteTask[A](task: Task[A], promise: Promise[A]): Task[Unit] = {
    task.compose(Transform.function[A, Unit] {
      case Success(v) =>
        promise.success(v)
        Task.Unit
      case Failure(cause) =>
        promise.failure(cause)
        Task.Unit
    })
  }
}
