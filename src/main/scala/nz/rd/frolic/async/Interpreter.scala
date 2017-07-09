package nz.rd.frolic.async

trait Interpreter {
  def run[A](t: Task[A]): Unit
}

trait InterpreterListener {
  def onStart(): Unit
  def onSuspend(): Unit
  def onResume(): Unit
  def onComplete(): Unit
}

object InterpreterListener {
  def nop = new InterpreterListener {
    override def onStart(): Unit = ()
    override def onComplete(): Unit = ()
    override def onSuspend(): Unit = ()
    override def onResume(): Unit = ()
  }
}