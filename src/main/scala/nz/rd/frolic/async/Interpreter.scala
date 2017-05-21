package nz.rd.frolic.async

import java.util.ArrayList
import java.util.concurrent.atomic.AtomicBoolean

import nz.rd.frolic.async.Task._

import scala.annotation.tailrec
import scala.util.control.NonFatal

trait Interpreter {
  def run(t: Task[_]): Unit
}

final object FunctionalInterpreter extends FunctionalInterpreter

class FunctionalInterpreter extends Interpreter {

  override def run(t: Task[_]): Unit = {
    def stepNoTailCall(t: Task[Any], stack: Transform.Stack[Any,Nothing]): Unit = step(t, stack)
    @tailrec // NOTE: Use 'Any' instead of proper types here so compiler can handle tailrec
    def step(t: Task[Any], stack: Transform.Stack[Any,Nothing]): Unit = t match {
      case result: Task.Completion[_] =>
        stack match {
          case Transform.Cons(head, tail) =>
            val next = try head.asInstanceOf[Any --> Any](result) catch {
              case NonFatal(t) => Failure(t)
            }
            step(next, tail)
          case Transform.Stop => ()
        }
      case e: Eval[_] =>
        val next = try Success(e()) catch {
          case NonFatal(t) => Failure(t)
        }
        step(next, stack)
      case fe: FlatEval[_] =>
        val next = try fe() catch {
          case NonFatal(t) => Failure(t)
        }
        step(next, stack)
      case AndThen(a, b) =>
        // Use local function to capture the type B
        def k[B](b: Task[B]): Transform[Any,B] = new Transform[Any,B] { def apply(t: Task.Completion[Any]) = b }
        step(a, k(b) +: stack)
      case Sequence(t, k) =>
        step(t, k.asInstanceOf[Any --> Any] +: stack)
      case s: Suspend[_] =>
        val k = new Continuation[Any] {
          private val called = new AtomicBoolean(false)
          private def stepWithCompletion(c: Completion[Any]): Unit = {
            if (called.compareAndSet(false, true)) {
              stepNoTailCall(c, stack)
            } else {
              throw new IllegalStateException("Continuation has already been called")
            }
          }
          override def resume(value: Any): Unit = stepWithCompletion(Task.Success(value))
          override def resumeWithException(cause: Throwable): Unit = stepWithCompletion(Task.Failure(cause))
        }
        try s.suspend(k) catch {
          case NonFatal(e) => k.resumeWithException(e) // Will throw IllegalStateException if k already completed
        }
    }
    step(t, Transform.Stop)
  }
}


private object ImperativeInterpreter {
  final class Fiber(var task: Task[_], var stack: AnyRef) extends Continuation[Any] {
    private def resume(): Unit = {
      @tailrec
      def loop(): Unit = {
        task match {
          case null => throw new IllegalStateException("Fiber has already completed")
          case d: Completion[_] =>
            pop() match {
              case f: Transform[_, _] =>
                val k = f.asInstanceOf[Transform[Any,_]]
                task = try k(d) catch {
                  case NonFatal(t) => Failure(t)
                }
                loop()
              case t: Task[_] =>
                task = t
                loop()
            }
          case e: Eval[_] =>
            task = try Success(e()) catch {
              case NonFatal(t) => Failure(t)
            }
            loop()
          case fe: FlatEval[_] =>
            task = try fe() catch {
              case NonFatal(t) => Failure(t)
            }
            loop()
          case AndThen(first, second) =>
            task = first
            stack match {
              case null =>
                stack = second
              case f: Transform[_, _] =>
                val l = new ArrayList[Any](4)
                l.add(f)
                l.add(second)
                stack = l
              case m: ArrayList[_] =>
                m.asInstanceOf[ArrayList[Any]].add(second)
            }
            loop()
          case Sequence(first, second) =>
            task = first
            push(second)
            loop()
          case s: Suspend[_] =>
            s.completion match {
              case Some(c) =>
                task = c
                loop()
              case None =>
                task = null
                s.suspend(this) // Will call this fiber to resume
            }
        }
      }
      loop()
    }

    // Continuation methods
    override def resume(value: Any): Unit = {
      assert(task == null)
      task = Task.Success(value)
      resume()
    }
    override def resumeWithException(cause: Throwable): Unit = {
      assert(task == null)
      task = Task.Failure(cause)
      resume()
    }

    // Stack manipulation
    def push(x: AnyRef): Unit = {
      stack match {
        case null =>
          stack = x
        case f: Transform[_, _] =>
          val l = new ArrayList[AnyRef](4)
          l.add(f)
          l.add(x)
          stack = l
        case m: ArrayList[_] =>
          m.asInstanceOf[ArrayList[AnyRef]].add(x)
      }
    }
    def pop(): AnyRef = {
      stack match {
        case null => null
        case l: ArrayList[_] if l.isEmpty =>
          null
        case l: ArrayList[_] =>
          l.asInstanceOf[ArrayList[AnyRef]].remove(l.size - 1)
        case x: AnyRef =>
          stack = null
          x
      }
    }
  }
}

class ImperativeInterpreter extends Interpreter {
  import ImperativeInterpreter.Fiber
  override def run(t: Task[_]): Unit = new Fiber(t, null).resume()
}