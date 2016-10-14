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
}

