package nz.rd.frolic.async

import java.util.ArrayList

import nz.rd.frolic.async.Task._

import scala.annotation.tailrec
import scala.util.control.NonFatal

trait Interpreter {
  def run(t: Task[_]): Unit
}

final object FunctionalInterpreter extends FunctionalInterpreter

class FunctionalInterpreter extends Interpreter {

  override def run(t: Task[_]): Unit = {
    def stepNoTailCall(t: Task[Any], stack: Continuation.Stack[Any,Nothing]): Unit = step(t, stack)
    @tailrec // NOTE: Use 'Any' instead of proper types here so compiler can handle tailrec
    def step(t: Task[Any], stack: Continuation.Stack[Any,Nothing]): Unit = t match {
      case result: Task.Result[_] =>
        stack match {
          case Continuation.Cons(head, tail) =>
            val next = try head.asInstanceOf[Any --> Any](result) catch {
              case NonFatal(t) => Throw(t)
            }
            step(next, tail)
          case Continuation.Stop => ()
        }
      case e: Eval[_] =>
        val next = try Value(e()) catch {
          case NonFatal(t) => Throw(t)
        }
        step(next, stack)
      case fe: FlatEval[_] =>
        val next = try fe() catch {
          case NonFatal(t) => Throw(t)
        }
        step(next, stack)
      case Sequence(t, k) =>
        step(t, k.asInstanceOf[Any --> Any] +: stack)
      case Suspend(suspender) =>
        // FIXME: Think about how to handle exceptions in suspender
        suspender { result: Result[_] => stepNoTailCall(result, stack) }
    }
    step(t, Continuation.Stop)
  }
}


private object ImperativeInterpreter {
  final class Fiber(var task: Task[_], var stack: AnyRef)
}

class ImperativeInterpreter extends Interpreter {
  import ImperativeInterpreter.Fiber
  override def run(t: Task[_]): Unit = resume(new Fiber(t, null))
  private def resume(fiber: Fiber): Unit = {
    @tailrec
    def loop(): Unit = {
      fiber.task match {
        case null => throw new IllegalStateException("Fiber has already completed")
        case d: Result[_] =>
          fiber.stack match {
            case null =>
              fiber.task = null
            case f: Continuation[_, _] =>
              val k = f.asInstanceOf[Continuation[Any,_]]
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
              val k = l.get(i).asInstanceOf[Continuation[Any,_]]
              l.remove(i)
              fiber.task = try k(d) catch {
                case NonFatal(t) => Throw(t)
              }
              loop()
          }
        case e: Eval[_] =>
          fiber.task = try Value(e()) catch {
            case NonFatal(t) => Throw(t)
          }
          loop()
        case fe: FlatEval[_] =>
          fiber.task = try fe() catch {
            case NonFatal(t) => Throw(t)
          }
          loop()
        case Sequence(first, second) =>
          fiber.task = first
          fiber.stack match {
            case null =>
              fiber.stack = second
            case f: Continuation[_, _] =>
              val l = new ArrayList[Any](4)
              l.add(f)
              l.add(second)
              fiber.stack = l
            case m: ArrayList[_] =>
              m.asInstanceOf[ArrayList[Any]].add(second)
          }
          loop()
        case Suspend(suspend) =>
          suspend { result: Task.Result[Any] =>
            fiber.task = result
            resume(fiber)
          }
      }
    }
    loop()
  }
}