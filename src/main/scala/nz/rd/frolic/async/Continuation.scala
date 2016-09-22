package nz.rd.frolic.async

trait Continuation[-A,+B] {
  def apply(d: Task.Done[A]): Task[B]
}
object Continuation {
  implicit def apply[A,B](f: Task.Done[A] => Task[B]) = new Continuation[A,B] {
    override def apply(d: Task.Done[A]) = f(d)
  }
  def lift[A,B](f: A => B) = new Continuation[A,B] {
    override def apply(d: Task.Done[A]) = d.map(f)
  }
}

