package nz.rd.frolic.async

trait AFunc[-A,+B] {
  def apply(arg: A): B
}

object AFunc {
  def fromFunction[A,B](f: A => B): AFunc[A, B] = new AFunc[A, B] {
    override def apply(arg: A): B = f(arg)
  }
}