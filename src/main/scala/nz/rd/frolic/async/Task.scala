package nz.rd.frolic.async

import scala.concurrent.{Future, Promise}

trait Task[+A] {
  import Task._

  def sequence[B](f: Result[A] => Task[B]): Task[B] = Sequence(this, Continuation(f))

  def map[B](f: A => B): Task[B] = sequence {
    case Value(v) => Value(f(v))
    case t@Throw(_) => t
  }
  def flatMap[B](f: A => Task[B]): Task[B] = sequence {
    case Value(v) => f(v)
    case t@Throw(_) => t
  }
  def foreach(f: A => Unit): Task[Unit] = map(f)
  def filter(f: A => Boolean): Task[A] = sequence {
    case r@Value(v) =>
      if (f(v)) r else Throw.Empty
    case t@Throw(_) => t
  }

  def liftDone: Task[Result[A]] = sequence(Task.Value(_))

  def flatten[B](implicit witness: A <:< Task[B]): Task[B] = sequence {
    case Value(v) => witness(v)
    case t@Throw(_) => t
  }
}

object Task {
  sealed trait Result[+A] extends Task[A]

  case class Value[+A](value: A) extends Result[A]
  object Value {
    val Unit = Value[Unit](())
  }
  case class Throw(throwable: Throwable) extends Result[Nothing]
  object Throw {
    val Empty = Throw(new NoSuchElementException("Task does not contain a value"))
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

  case class Sequence[A,+B](t: Task[A], k: (A --> B)) extends Task[B]
  case class Suspend[A,+B](f: (Result[A] => Unit) => Unit) extends Task[B]

//  case class Schedule[+A](s: Scheduling, t: Task[A]) extends Task[A]
//  case class Spawn[+A](child: Task[_], k: Task[A]) extends Task[A]

  def toFuture[A](t: Task[A]): (Task[Unit], Future[A]) = {
    val promise = Promise[A]()
    val promiseTask: Task[Unit] = t.sequence {
      case Value(v) =>
        promise.success(v)
        Value.Unit
      case t@Throw(cause) =>
        promise.failure(cause)
        Value.Unit
    }
    (promiseTask, promise.future)
  }

}