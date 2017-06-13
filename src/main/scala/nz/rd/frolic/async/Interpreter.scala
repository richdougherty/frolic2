package nz.rd.frolic.async

trait Interpreter {
  def run[A](t: Task[A]): Unit
}

