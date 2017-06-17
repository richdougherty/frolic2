package nz.rd.frolic.async

import scala.concurrent.{Future, Promise}

trait Task[+A] {
  import Task._

  def compose[B](tr: A --> B): Task[B] = Compose(this, tr)

  def map[B](f: A => B): Task[B] = compose(Transform.mapped(f))
  def flatMap[B](f: A => Task[B]): Task[B] = Flatten(map(f))

  def foreach(f: A => Unit): Task[Unit] = map(f)
  def filter(f: A => Boolean): Task[A] = flatMap { x: A =>
    if (f(x)) Success(x) else Failure(new NoSuchElementException("No result after filtering"))
  }

  def flatten[B](implicit witness: A <:< Task[B]): Task[B] = Flatten[B](this.asInstanceOf[Task[Task[B]]])
  def liftCompletion: Task[Completion[A]] = compose { c: Completion[A] => Task.Success(c) }

  // Higher level constructs

  def `then`[B](block: => B): Task[B] = thenTask(Eval(block))
  def flatThen[B](block: => Task[B]): Task[B] = thenTask(Flatten(Eval(block)))
  def thenTask[B](t: Task[B]): Task[B] = compose(Transform.fixed(t))

  def `finally`(block: => Any): Task[A] = finallyTask(Eval(block))
  def flatFinally(taskBlock: => Task[Any]): Task[A] = finallyTask(Flatten(Eval(taskBlock)))
  def finallyTask(t: Task[Any]): Task[A] = this.compose(Transform.function[A,A]({ tryCompletion: Completion[A] =>
    t.compose(Transform.function[Any,A]({
      case Success(_) => tryCompletion
      case f@Failure(_) => f // Use Failure in finally block, based on Java exception semantics
    }))
  }))

  def `catch`[B >: A](pf: PartialFunction[Throwable, B]): Task[B] = flatCatch(pf.andThen(Success(_)))
  def `flatCatch`[B >: A](pf: PartialFunction[Throwable, Task[B]]): Task[B] = this.compose(Transform.function({
    case _@Failure(cause) if pf.isDefinedAt(cause) =>
      pf(cause)
    case x => x
  }))
}

final object Task {

  sealed trait Completion[+A] extends Task[A]

  case class Success[+A](value: A) extends Completion[A]
  object Success {
    val Unit = Success[Unit](())
  }
  case class Failure(throwable: Throwable) extends Completion[Nothing]
  object Failure {
    val Empty = Failure(new NoSuchElementException("Task does not contain a value"))
  }

  trait Eval[+A] extends Task[A] {
    def completed: Boolean
    def apply(): A
  }
  object Eval {
    def apply[A](body: => A): Eval[A] = new Eval[A] {
      override def completed: Boolean = false
      override def apply(): A = body
    }
  }

  case class Flatten[+A](t: Task[Task[A]]) extends Task[A]

  case class Compose[A,+B](a: Task[A], b: Transform[A,B]) extends Task[B]

  trait Suspend[A] extends Task[A] {
    def suspend(resume: Continuation[A]): Unit
  }
  object Suspend {
    def apply[A](f: Continuation[A] => Unit): Suspend[A] = new Suspend[A] {
      override def suspend(resume: Continuation[A]): Unit = f(resume)
    }
  }

  /**
   * Creates a task and a future value of its result. Wraps an existing task, extending it,
   * so that when the new task is executed it will complete the future as a side effect. To
   * get the future value you must still execute the task.
   */
  def toFutureTask[A](t: Task[A]): (Task[A], Future[A]) = {
    val promise = Promise[A]()
    val promiseTask: Task[A] = t.compose(Transform.function[A,A] {
      case s@Success(v) =>
        promise.success(v)
        s
      case f@Failure(cause) =>
        promise.failure(cause)
        f
    })
    (promiseTask, promise.future)
  }

  // TODO: Improve perf by avoiding allocation when no simplification occurs
  def simplify[A](t: Task[A]): Task[A] = t match {
    case c: Completion[A] => c
    case Compose(a, transform) =>
      Compose(simplify(a), transform) match {
        case Compose(_: Completion[_], Transform.Fixed(fixedTask)) => simplify(fixedTask)
        case Compose(failure@Failure(_), Transform.Map(_)) => failure
        case compose => compose
      }
    case Flatten(t) =>
      Flatten(simplify(t)) match {
        case Flatten(Success(t)) => t.asInstanceOf[Task[A]]
        case x => x.asInstanceOf[Task[A]]
      }
    case e: Eval[A] if e.completed =>
      Success(e.apply())
    case x => x
  }

}