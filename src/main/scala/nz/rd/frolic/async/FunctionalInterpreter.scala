package nz.rd.frolic.async

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import scala.annotation.tailrec
import scala.util.control.NonFatal

final object FunctionalInterpreter extends FunctionalInterpreter

class FunctionalInterpreter extends Interpreter {

  private val runCounter = new AtomicLong()

  override def run[A](task: Task[A]): Unit = {

    val runId = runCounter.getAndIncrement()

    val nop: Transform[A,A] = Transform.Function(identity[Task[A]], identity[Task[A]])

    def log(msg: String): Unit = {
      println(s"[$runId] $msg")
    }

    @tailrec
    def step(task: Task[A]): Unit = task match {
      case Task.Compose(composedTask, transform) =>
        log(s"Stepping Task.Compose($composedTask, $transform)")
        stepComposedTask(composedTask, transform) match {
          case Some(nextTask) =>
            log(s"Got step of $nextTask")
            step(nextTask)
          case None => ()
        }
      case _ =>
        // Normalize into a Compose
        log("Can't step Task that isn't a Compose, wrapping with Compose then stepping again")
        step(Task.Compose(task, nop))
    }

    def stepNoTailCall(t: Task[A]): Unit = step(t)

    def stepComposedTask[B](task: Task[B], transform: Transform[B,A]): Option[Task[A]] = task match {

      case compose: Task.Compose[_, _] =>
        // Convert the Compose into a normal form with only one level of Compose and with Transforms joined by Cons
        log("Can't step nested Compose, moving nested task to head position, consing nested transform, then stepping again")
        Some(Task.Compose(compose.task, Transform.Compose(compose.transform, transform)))

      case eval: Task.Eval[_] =>
        // Evaluate the function
        log("Running Eval")
        val nextTask = eval.completion match {
          case None =>
            println("Eval not complete: evaluating now")
            try Task.Success(eval.apply()) catch {
              case NonFatal(t) => Task.Failure(t)
            }
          case Some(completion) =>
            println(s"Eval is already complete: $completion")
            completion
        }
        Some(Task.Compose(nextTask, transform))


      case flatten: Task.Flatten[_] =>
        log("Converting Flatten to equivalent Compose/Transform")
        val flattenB: Task.Flatten[B] = flatten.asInstanceOf[Task.Flatten[B]]
        val unflattenedTask: Task[Task[B]] = flattenB.task
        val flattenTransform: Task[B] --> B = Transform.Function.successFlatMap(identity[Task[B]])
        val flattened: Task[B] = unflattenedTask.compose(flattenTransform)
        Some(Task.Compose(flattened, transform))

      case suspend: Task.Suspend[_] =>
        // Create a continuation to resume with the result
        log("Suspending Suspend task")
        val resume: Continuation[B] = new Continuation[B] {
          private val called = new AtomicBoolean(false)

          private def stepWithCompletion(c: Task.Completion[B]): Unit = {
            if (called.compareAndSet(false, true)) {
              stepNoTailCall(Task.Compose(c, transform))
            } else {
              throw new IllegalStateException("Continuation has already been called")
            }
          }

          override def resume(value: B): Unit = {
            log(s"Resume called on suspended task with value $value")
            stepWithCompletion(Task.Success(value))
          }

          override def resumeWithException(cause: Throwable): Unit = {
            log(s"Resume called on suspended task with exception $cause")
            stepWithCompletion(Task.Failure(cause))
          }
        }
        // The interpreter suspends here. It will start again when resume is called.
        try suspend.asInstanceOf[Task.Suspend[B]].suspend(resume) catch {
          case NonFatal(e) => resume.resumeWithException(e) // Will throw IllegalStateException if k already completed
        }
        None

      case completion: Task.Completion[_] =>
        log("Composed task was complete, passing completion to transformation")
        transform match {
          case `nop` => None
          case Transform.Compose(left, right) =>
            stepConsedTransform(completion, left, right)
          case _ => stepConsedTransform(completion, transform, nop)
        }
    }

    def stepConsedTransform[B,C](completion: Task.Completion[B], transform: Transform[B,C], nextTransform: Transform[C,A]): Option[Task[A]] = transform match {

      case Transform.Compose(a, b) =>
        // Rearrange cons to a normalized form
        log("Running Cons")
        Some(Task.Compose(completion, Transform.Compose(a, Transform.Compose(b, nextTransform))))

      case f: Transform.Function[_,_] =>
        log("Running Function")
        Some(Task.Compose(try f(completion) catch { case NonFatal(t) => Task.Failure(t) }, nextTransform))

    }

    step(task)
  }

}