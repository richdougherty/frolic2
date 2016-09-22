package nz.rd.frolic.async

import java.util.ArrayList

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal

trait Task[+A] {
  import Task._

  def sequence[B](f: Done[A] => Task[B]): Task[B] = Sequence(this, f)

  def map[B](f: A => B): Task[B] = sequence {
    case Return(v) => Return(f(v))
    case t@Throw(_) => t
  }
  def flatMap[B](f: A => Task[B]): Task[B] = sequence {
    case Return(v) => f(v)
    case t@Throw(_) => t
  }
  def foreach(f: A => Unit): Task[Unit] = map(f)
  def filter(f: A => Boolean): Task[A] = sequence {
    case r@Return(v) =>
      if (f(v)) r else Empty
    case t@Throw(_) => t
  }

  def liftDone: Task[Done[A]] = sequence(Task.Return(_))

  def flatten[B](implicit witness: A <:< Task[B]): Task[B] = sequence {
    case Return(v) => witness(v)
    case t@Throw(_) => t
  }
}

object Task {
  sealed trait Done[+A] extends Task[A]

  case class Return[+A](value: A) extends Done[A]
  case class Throw(throwable: Throwable) extends Done[Nothing]

  case class Do[+A](f: () => Task[A]) extends Task[A]
  case class Sequence[A,+B](first: Task[A], second: Done[A] => Task[B]) extends Task[B]
//  case class Schedule[+A](s: Scheduling, t: Task[A]) extends Task[A]
//  case class Spawn[+A](child: Task[_], k: Task[A]) extends Task[A]

  val Unit = Return[Unit](())
  val Empty = Throw(new NoSuchElementException("Task does not contain a value"))

  private final class Fiber(var task: Task[_], var stack: AnyRef)

  def run(t: Task[_]): Unit = {
    val fiber: Fiber = new Fiber(t, null)
    @tailrec
    def loop(): Unit = {
      fiber.task match {
        case null => throw new IllegalStateException("Fiber has already completed")
        case d: Done[_] =>
          fiber.stack match {
            case null =>
              fiber.task = null
            case f: Function1[_, _] =>
              val k = f.asInstanceOf[Done[Any] => Task[_]]
              fiber.task = try k(d) catch {
                case NonFatal(t) => Throw(t)
              }
              fiber.stack = null
              loop()
            case l: ArrayList[_] if l.isEmpty =>
              fiber.stack = null
              ()
            case l: ArrayList[_] =>
              val i: Int = l.size - 1
              val k = l.get(i).asInstanceOf[Done[Any] => Task[_]]
              l.remove(i)
              fiber.task = try k(d) catch {
                case NonFatal(t) => Throw(t)
              }
              loop()
          }
        case Do(f) =>
          fiber.task = try f() catch {
            case NonFatal(t) => Throw(t)
          }
          loop()
        case Sequence(first, second) =>
          fiber.task = first
          fiber.stack match {
            case null =>
              fiber.stack = second
            case f: Function1[_, _] =>
              val l = new ArrayList[AnyRef](4)
              l.add(f)
              l.add(second)
              fiber.stack = l
            case m: ArrayList[_] =>
              m.asInstanceOf[ArrayList[_ => _]].add(second)
          }
          loop()
      }
    }
    loop()
  }

  def runToFuture[A](t: Task[A]): Future[A] = {
    val p = Promise[A]()
    run(t.sequence {
      case Return(v) =>
        p.success(v)
        Unit
      case t@Throw(cause) =>
        p.failure(cause)
        Unit
    })
    p.future
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