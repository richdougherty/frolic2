package nz.rd.frolic.async

import java.util.ArrayList

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

trait Task[+A] {
  import Task._

  def andThen[B](f: Result[A] => Task[B]): Task[B] = Sequence(this, Continuation(f))

  def map[B](f: A => B): Task[B] = andThen {
    case Value(v) => Value(f(v))
    case t@Throw(_) => t
  }
  def flatMap[B](f: A => Task[B]): Task[B] = andThen {
    case Value(v) => f(v)
    case t@Throw(_) => t
  }
  def foreach(f: A => Unit): Task[Unit] = map(f)
  def filter(f: A => Boolean): Task[A] = andThen {
    case r@Value(v) =>
      if (f(v)) r else Throw.Empty
    case t@Throw(_) => t
  }

  def liftDone: Task[Result[A]] = andThen(Task.Value(_))

  def flatten[B](implicit witness: A <:< Task[B]): Task[B] = andThen {
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

  case class DoFunc[+A](f: () => A) extends Task[A]
  case class DoTask[+A](f: () => Task[A]) extends Task[A]
  case class Sequence[A,+B](t: Task[A], k: (A --> B)) extends Task[B]
  case class Suspend[A,+B](f: (Result[A] => Unit) => Unit) extends Task[B]


//  case class Schedule[+A](s: Scheduling, t: Task[A]) extends Task[A]
//  case class Spawn[+A](child: Task[_], k: Task[A]) extends Task[A]
//
//  private final class Fiber(var task: Task[_], var stack: AnyRef)
//
//  def run(t: Task[_]): Unit = {
//    val fiber: Fiber = new Fiber(t, null)
//    @tailrec
//    def loop(): Unit = {
//      fiber.task match {
//        case null => throw new IllegalStateException("Fiber has already completed")
//        case d: Result[_] =>
//          fiber.stack match {
//            case null =>
//              fiber.task = null
//            case f: Continuation[_, _] =>
//              val k = f.asInstanceOf[Continuation[Any,_]]
//              fiber.task = try k(d) catch {
//                case NonFatal(t) => Throw(t)
//              }
//              fiber.stack = null
//              loop()
//            case l: ArrayList[_] if l.isEmpty =>
//              fiber.stack = null
//              ()
//            case l: ArrayList[_] =>
//              val i: Int = l.size - 1
//              val k = l.get(i).asInstanceOf[Continuation[Any,_]]
//              l.remove(i)
//              fiber.task = try k(d) catch {
//                case NonFatal(t) => Throw(t)
//              }
//              loop()
//          }
//        case DoFunc(f) =>
//          fiber.task = try Value(f()) catch {
//            case NonFatal(t) => Throw(t)
//          }
//          loop()
//        case DoTask(f) =>
//          fiber.task = try f() catch {
//            case NonFatal(t) => Throw(t)
//          }
//          loop()
//        case Sequence(first, second) =>
//          fiber.task = first
//          fiber.stack match {
//            case null =>
//              fiber.stack = second
//            case f: Continuation[_, _] =>
//              val l = new ArrayList[Any](4)
//              l.add(f)
//              l.add(second)
//              fiber.stack = l
//            case m: ArrayList[_] =>
//              m.asInstanceOf[ArrayList[Any]].add(second)
//          }
//          loop()
//      }
//    }
//    loop()
//  }

  def toFuture[A](t: Task[A]): (Task[Unit], Future[A]) = {
    val promise = Promise[A]()
    val promiseTask: Task[Unit] = t.andThen {
      case Value(v) =>
        promise.success(v)
        Value.Unit
      case t@Throw(cause) =>
        promise.failure(cause)
        Value.Unit
    }
    (promiseTask, promise.future)
  }

    //  private final case class Fiber(var task: Task[_], var stack: AnyRef) {
//    @tailrec
//    def run(): Unit = {
//      task match {
//        case null => throw new IllegalStateException("Fiber has already completed")
//        case d: Done[_] =>
//          stack match {
//            case null =>
//              task = null
//            case f: (_ => _) =>
//              val k = f.asInstanceOf[Done[Any] => Task[_]]
//              task = try k(d) catch {
//                case NonFatal(t) => Throw(t)
//              }
//              run()
//            case l: ArrayList[_] if l.isEmpty =>
//              stack = null
//              ()
//            case l: ArrayList[_] =>
//              val i: Int = l.size - 1
//              val k = l.get(i).asInstanceOf[Done[Any] => Task[_]]
//              l.remove(i)
//              task = try k(d) catch {
//                case NonFatal(t) => Throw(t)
//              }
//              run()
//          }
//        case Do(f) =>
//          task = try f() catch {
//            case NonFatal(t) => Throw(t)
//          }
//          run()
//        case Sequence(first, second) =>
//          task = first
//          stack match {
//            case null =>
//              stack = second
//            case f: (_ => _) =>
//              val l = new ArrayList[AnyRef](4)
//              l.add(f)
//              l.add(second)
//              stack = l
//            case m: ArrayList[_] =>
//              m.asInstanceOf[ArrayList[_ => _]].add(second)
//          }
//          run()
//      }
//    }
//  }
//
//  def run[A](t: Task[A]): Unit = {
//
//
//    t match {
//      case r@Return(_) => k(r)
//      case t@Throw(_) => k(t)
//      case Do(f) => {
//        val next: Task[_] = try f() catch {
//          case NonFatal(t) => Throw(t)
//        }
//        run(next, k)
//      }
//      case Sequence(first, second) =>
//        run(first, sequenceK(first, second, k))
//    }
//  }
//  private def sequenceK[A, B](first: Task[A], second: Done[A] => Task[B], k: Done[B] => _): Done[A] => _ = {
//    { firstResult: Done[A] =>
//      val secondTask: Task[B] = second(firstResult)
//      run(secondTask, k)
//    }
//  }

//  def runToFuture[A](t: Task[A]): Future[A] = {
//    val promise = Promise[A]()
//    run
//    promise.future
//  }


}