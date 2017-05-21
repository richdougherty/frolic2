package nz.rd.frolic.async

trait Transform[-A,+B] {
  def apply(d: Task.Completion[A]): Task[B]
}
object Transform {
  implicit def f[A,B](f: A => Task[B]) = new Transform[A,B] {
    override def apply(d: Task.Completion[A]) = d match {
      case Task.Success(a) => f(a)
      case t: Task.Failure => t
    }
  }
  implicit def apply[A,B](f: Task.Completion[A] => Task[B]) = new Transform[A,B] {
    override def apply(d: Task.Completion[A]) = f(d)
  }
  def lift[A,B](f: A => B) = new Transform[A,B] {
    override def apply(d: Task.Completion[A]) = d.map(f)
  }

  sealed trait Stack[A,B] {
    def +:[C,A1<:A](k: Transform[C,A1]): Stack[C,B] = Cons[C,A,B](k, this)
  }
  case class Cons[A,B,C](head: Transform[A,B], tail: Stack[B,C]) extends Stack[A,C]
  case object Stop extends Stack[Any,Nothing]
}

