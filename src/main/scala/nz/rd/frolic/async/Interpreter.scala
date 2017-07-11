package nz.rd.frolic.async

trait Interpreter {
  def run[A](t: Task[A]): Unit
}

trait InterpreterListener {
  type ActiveData
  type SuspendingData

  def starting(): ActiveData
  def suspending(startResumeData: ActiveData): SuspendingData
  def suspended(suspendingData: SuspendingData): Unit
  def resuming(): ActiveData
  def completing(startOrResumeData: ActiveData): Unit
}

object InterpreterListener {
  def nop = new InterpreterListener {
    type ActiveData = Unit
    type SuspendingData = Unit
    override def starting(): Unit = ()
    override def suspending(activeData: Unit): Unit = ()
    override def suspended(suspendingData: Unit): Unit = ()
    override def resuming(): Unit = ()
    override def completing(activeData: Unit): Unit = ()
  }
}