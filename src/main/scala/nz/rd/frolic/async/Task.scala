package nz.rd.frolic.async

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}

trait Task[+A] {
  import Task._

  def map[B](f: A => B): Task[B] = sequence {
    case Success(v) => Success(f(v))
    case t@Failure(_) => t
  }
  def flatMap[B](f: A => Task[B]): Task[B] = sequence {
    case Success(v) => f(v)
    case t@Failure(_) => t
  }
  def foreach(f: A => Unit): Task[Unit] = map(f)
  def filter(f: A => Boolean): Task[A] = sequence {
    case r@Success(v) =>
      if (f(v)) r else Failure.Empty
    case t@Failure(_) => t
  }
  // Note different signature/behavior to Future.andThen, since this accepts successful values only
  def andThen[B](t: Task[B]): Task[B] = AndThen(this, t)

  //

  def flatten[B >: A](implicit witness: A <:< Task[B]): Task[B] = Flatten(this)
  def unflatten: Task[Completion[A]] = sequence(Task.Success(_))



  def sequence[B](f: Completion[A] => Task[B]): Task[B] = Sequence(this, Transform(f))

  // Higher level constructs

  def `finally`(block: => Any): Task[A] = sequence(Eval(block).andThen(_))
  def `flatFinally`(block: => Task[Any]): Task[A] = sequence(FlatEval(block).andThen(_))
  def `catch`[B >: A](pf: PartialFunction[Throwable, B]): Task[B] = sequence {
    case _@Failure(cause) if pf.isDefinedAt(cause) =>
      Success(pf(cause))
    case x => x
  }
  def `flatCatch`[B >: A](pf: PartialFunction[Throwable, Task[B]]): Task[B] = sequence {
    case _@Failure(cause) if pf.isDefinedAt(cause) =>
      pf(cause)
    case x => x
  }

}

final object Task {

  // FIXME: Think about whether this hack is worth it!
  //implicit def unitConversion(t: Task[_]): Task[Unit] = t.asInstanceOf[Task[Unit]]

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
    def apply(): A
  }
  object Eval {
    def apply[A](body: => A): Eval[A] = new Eval[A] {
      def apply(): A = body
    }
  }

  trait FlatEval[+A] extends Task[A] {
    def apply(): Task[A]
  }
  object FlatEval {
    def apply[A](body: => Task[A]): FlatEval[A] = new FlatEval[A] {
      def apply(): Task[A] = body
    }
  }

  case class Flatten[A,+B >: A](t: Task[A]) extends Task[B]
  case class AndThen[+A,+B](a: Task[A], b: Task[B]) extends Task[B]
  case class Sequence[A,+B](t: Task[A], k: (A --> B)) extends Task[B]

  trait Suspend[+A] extends Task[A] {
    def completion: Option[Completion[A]]
    def suspend(k: Continuation[A]): Unit
  }
  object Suspend {
    def apply[A](f: Continuation[A] => Unit): Suspend[A] = new Suspend[A] {
      override def completion: Option[Completion[A]] = None
      override def suspend(k: Continuation[A]): Unit = f(k)
    }
  }

//  case class Schedule[+A](s: Scheduling, t: Task[A]) extends Task[A]
//  case class Spawn[+A](child: Task[_], k: Task[A]) extends Task[A]

  /**
   * Creates a task and a future value of its result. Wraps an existing task, extending it,
   * so that when the new task is executed it will complete the future as a side effect. To
   * get the future value you must still execute the task.
   */
  def toFutureTask[A](t: Task[A]): (Task[A], Future[A]) = {
    val promise = Promise[A]()
    val promiseTask: Task[A] = t.sequence {
      case s@Success(v) =>
        promise.success(v)
        s
      case f@Failure(cause) =>
        promise.failure(cause)
        f
    }
    (promiseTask, promise.future)
  }

  // TODO: Improve perf by avoiding allocation when no simplification occurs
  def simplify[A](t: Task[A]): Task[A] = t match {
    case c: Completion[A] => c
    case AndThen(a, b) =>
      AndThen(simplify(a), simplify(b)) match {
        case AndThen(Success(_), b) => b
        case AndThen(f@Failure(_), _) => f
        case x => x
      }
    case Flatten(t) =>
      Flatten(simplify(t)) match {
        case Flatten(Success(t)) => t.asInstanceOf[Task[A]]
        case x => x.asInstanceOf[Task[A]]
      }
    case Sequence(t, k) =>
      Sequence(simplify(t), k)
    case s: Suspend[A] =>
      s.completion match {
        case None => s
        case Some(c) => c
      }
    case x => x
  }

}