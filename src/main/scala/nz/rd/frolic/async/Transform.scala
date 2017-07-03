package nz.rd.frolic.async

sealed trait Transform[-A,+B]

object Transform {

  case class Compose[-A,B,+C](left: Transform[A,B], right: Transform[B,C]) extends Transform[A,C]

  trait Function[-A,+B] extends Transform[A,B] {
    def success: Task.Success[A] => Task[B]
    def failure: Task.Failure => Task[B]

    def apply(completion: Task.Completion[A]): Task[B] = completion match {
      case s@Task.Success(_) => success(s)
      case f@Task.Failure(_) => failure(f)
    }
  }

  object Function {

    class DefaultFunction[A,B](
        override val success: Task.Success[A] => Task[B],
        override val failure: Task.Failure => Task[B]) extends Function[A,B]

    def apply[A,B](
        success: Task.Success[A] => Task[B],
        failure: Task.Failure => Task[B]): Function[A,B] = new DefaultFunction(success, failure)

    def unapply[A,B](f: Function[A,B]): Option[(Task.Success[A] => Task[B], Task.Failure => Task[B])] = {
      Some((f.success, f.failure))
    }

    private class CompletionFunction[A,B](completion: Task.Completion[A] => Task[B]) extends Function[A,B] {
      override def apply(c: Task.Completion[A]): Task[B] = completion(c)
      override def success: Task.Success[A] => Task[B] = completion
      override def failure: Task.Failure => Task[B] = completion
    }

    def fromCompletion[A,B](c: Task.Completion[A] => Task[B]): Function[A,B] = new CompletionFunction(c)

    class SuccessFunction[-A,+B](override val success: Task.Success[A] => Task[B]) extends Function[A,B] {
      final override val failure: Task.Failure => Task[B] = identity[Task.Failure]
    }
    class SuccessMap[-A,+B](f: A => B) extends SuccessFunction[A,B]((s: Task.Success[A]) => Task.Success(f(s.value)))
    class SuccessFlatMap[-A,+B](f: A => Task[B]) extends SuccessFunction[A,B]((s: Task.Success[A]) => f(s.value))
    case class SuccessTask[+B](task: Task[B]) extends SuccessFunction[Any,B]((_: Task.Success[Any]) => task)

    class FailureFunction[A](override val failure: Task.Failure => Task[A]) extends Function[A,A] {
      final override val success: Task.Success[A] => Task[A] = identity[Task[A]]
    }
    case class FailureTask[A](task: Task[A]) extends FailureFunction[A]((_: Task.Failure) => task)
    class FailureMap[A](f: Throwable => Throwable)
        extends FailureFunction[A]((failure: Task.Failure) => Task.Failure(f(failure.throwable)))
    class FailureFlatMap[A](f: Throwable => Task[A])
        extends FailureFunction[A]((failure: Task.Failure) => f(failure.throwable))

    case class FixedTask[A](task: Task[A]) extends DefaultFunction[Any,A](
      (_: Task.Success[Any]) => task,
      (_: Task.Failure) => task
    )

    // TODO: class AlwaysTask[A](task: Task[A]) - always do a particular task, no matter the completion?

    def successMap[A,B](f: A => B): Function[A,B] = new SuccessMap(f)
    def successFlatMap[A,B](f: A => Task[B]): Function[A,B] = new SuccessFlatMap(f)
    def failureMap[A](f: Throwable => Throwable): Function[A,A] = new FailureMap[A](f)
  }

}