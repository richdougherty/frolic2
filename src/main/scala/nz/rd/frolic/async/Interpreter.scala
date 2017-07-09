package nz.rd.frolic.async

trait Interpreter {
  def run[A](t: Task[A]): Unit
}

trait InterpreterListener {
  def afterStart(): Unit
  def beforeSuspend(): Unit
  def afterResume(): Unit
  def beforeComplete(): Unit
}