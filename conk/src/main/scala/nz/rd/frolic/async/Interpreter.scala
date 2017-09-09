package nz.rd.frolic.async

trait Interpreter {
  def run[A](t: Task[A]): Unit
}

trait InterpreterListener {
  type ActiveData
  type SuspendingData
  type ResumingData

  def starting(): ActiveData
  def suspending(activeData: ActiveData): SuspendingData
  def suspended(suspendingData: SuspendingData): Unit
  def resuming(): ResumingData
  def resumed(resumingData: ResumingData): ActiveData
  def completing(activeData: ActiveData): Unit
}

object InterpreterListener {
  def nop = new InterpreterListener {
    type ActiveData = Unit
    type SuspendingData = Unit
    type ResumingData = Unit
    override def starting(): Unit = ()
    override def suspending(activeData: Unit): Unit = ()
    override def suspended(suspendingData: Unit): Unit = ()
    override def resuming(): Unit = ()
    override def resumed(resumingData: Unit): Unit = ()
    override def completing(activeData: Unit): Unit = ()
  }
}