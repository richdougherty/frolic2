package nz.rd.frolic.async

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

import scala.annotation.tailrec
import scala.util.control.NonFatal

class FunctionalInterpreter(listener: InterpreterListener) extends Interpreter {

  private val runCounter = new AtomicLong()

  override def run[A](task: Task[A]): Unit = {

    val runId = runCounter.getAndIncrement()

    val nop: Transform[A,A] = Transform.Function(identity[Task[A]], identity[Task[A]])

    def log(msg: String): Unit = {
      //println(s"[$runId] $msg")
    }

    @tailrec
    def step(task: Task[A], listenerData: listener.ActiveData): Unit = task match {
      case Task.Compose(composedTask, transform) =>
        log(s"Stepping Task.Compose($composedTask, $transform)")
        stepComposedTask(composedTask, transform, listenerData) match {
          case Some(nextTask) =>
            log(s"Got step of $nextTask")
            step(nextTask, listenerData)
          case None =>
            listener.completing(listenerData)
            ()
        }
      case _ =>
        // Normalize into a Compose
        log("Can't step Task that isn't a Compose, wrapping with Compose then stepping again")
        step(Task.Compose(task, nop), listenerData)
    }

    def stepNoTailCall(t: Task[A], listenerData: listener.ActiveData): Unit = step(t, listenerData)

    def stepComposedTask[B](task: Task[B], transform: Transform[B,A], listenerData: listener.ActiveData): Option[Task[A]] = task match {

      case compose: Task.Compose[_, _] =>
        // Convert the Compose into a normal form with only one level of Compose and with Transforms joined by Cons
        log("Can't step nested Compose, moving nested task to head position, consing nested transform, then stepping again")
        Some(Task.Compose(compose.task, Transform.Compose(compose.transform, transform)))

      case eval: Task.Eval[_] =>
        // Evaluate the function
        log("Running Eval")
        val nextTask = eval.completion match {
          case None =>
            log("Eval not complete: evaluating now")
            try Task.Success(eval.apply()) catch {
              case NonFatal(t) => Task.Failure(t)
            }
          case Some(completion) =>
            log(s"Eval is already complete: $completion")
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
        log("Suspending Suspend task")
        val suspendingData: listener.SuspendingData = listener.suspending(listenerData)

        // Create a continuation to resume with the result
        val resume: Continuation[B] = new Continuation[B] {
          private val suspendedContext = Context.current
          private val called = new AtomicBoolean(false)

          override def resumeWithThunk(thunk: => Task.Completion[B]): Unit = {
            if (called.compareAndSet(false, true)) {
              val threadContext = Context.current
              Context.current = suspendedContext
              try {
                val resumingData = listener.resuming()
                val completion: Task.Completion[B] = thunk
                listener.resumed(resumingData)
                stepNoTailCall(Task.Compose(completion, transform), listenerData)
              } finally Context.current = threadContext
            } else {
              throw new IllegalStateException(s"Task.Suspend's Continuation has already been called.")
            }
          }

          override def resume(value: B): Unit = {
            log(s"Resume called on suspended task with value $value")
            resumeWithThunk(Task.Success(value))
          }

          override def resumeWithException(cause: Throwable): Unit = {
            log(s"Resume called on suspended task with exception $cause")
            resumeWithThunk(Task.Failure(cause))
          }
        }
        // The interpreter suspends here. It will start again when resume is called.
        try suspend.asInstanceOf[Task.Suspend[B]].suspend(resume) catch {
          case NonFatal(e) =>
            System.err.println("Error occurred during suspend operation, attempting to resume with error. Will fail if suspend has also resumed.")
            e.printStackTrace()
            resume.resumeWithException(e) // Will throw IllegalStateException if k already completed
        }
        listener.suspended(suspendingData)
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

    val oldContext: Context = Context.current
    Context.current = Context.empty
    try {
      val data: listener.ActiveData = listener.starting()
      step(task, data)
    } finally Context.current = oldContext
  }

}