package nz.rd.frolic.async

trait Continuation[-A,+B] {
  def apply(d: Task.Result[A]): Task[B]
}
object Continuation {
  implicit def f[A,B](f: A => Task[B]) = new Continuation[A,B] {
    override def apply(d: Task.Result[A]) = d match {
      case Task.Value(a) => f(a)
      case t: Task.Throw => t
    }
  }
  implicit def apply[A,B](f: Task.Result[A] => Task[B]) = new Continuation[A,B] {
    override def apply(d: Task.Result[A]) = f(d)
  }
  def lift[A,B](f: A => B) = new Continuation[A,B] {
    override def apply(d: Task.Result[A]) = d.map(f)
  }

  sealed trait Stack[A,B] {
    def +:[C,A1<:A](k: Continuation[C,A1]): Stack[C,B] = Cons[C,A,B](k, this)
  }
  case class Cons[A,B,C](head: Continuation[A,B], tail: Stack[B,C]) extends Stack[A,C]
  case object Stop extends Stack[Any,Nothing]
}

