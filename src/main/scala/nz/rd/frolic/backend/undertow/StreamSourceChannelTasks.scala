package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import nz.rd.frolic.async.Task
import org.xnio.ChannelListener
import org.xnio.channels.StreamSourceChannel

object StreamSourceChannelTasks {
  def apply(channel: => StreamSourceChannel): StreamSourceChannelTasks =
    new StreamSourceChannelTasks(() => channel)
}

class StreamSourceChannelTasks private[StreamSourceChannelTasks] (getChannel: () => StreamSourceChannel) {
  private var channel: StreamSourceChannel = null

  private def readTask[A](f: StreamSourceChannel => A, needsCallback: A => Boolean): Task[A] = {
    if (channel == null) { channel = getChannel() }
    val result: A = f(channel)
    if (needsCallback(result)) {
      def scheduleCallback(resume: (Task.Value[A] => Unit)): Unit = {
        channel.getReadSetter.set(new ChannelListener[StreamSourceChannel] {
          override def handleEvent(channel: StreamSourceChannel): Unit = {
            channel.suspendReads() // PERF: Can we avoid suspending reads?
            val result: A = f(channel)
            if (needsCallback(result)) {
              scheduleCallback(resume)
            } else {
              resume(Task.Value(result))
            }
          }
        })
      }
      Task.Suspend { resume: (Task.Value[A] => Unit) => scheduleCallback(resume) }
    } else {
      Task.Value(result)
    }
  }

  def read(dst: ByteBuffer): Task[Int] =
    readTask(_.read(dst), (_: Int) == 0)
  def read(dsts: Array[ByteBuffer]): Task[Long] =
    readTask(_.read(dsts), (_: Long) == 0L)
  def read(dsts: Array[ByteBuffer], offset: Int, length: Int): Task[Long] =
    readTask(_.read(dsts, offset, length), (_: Long) == 0L)

  def close(): Unit = channel.close()
  def isOpen(): Boolean = channel.isOpen

  def waitForClose(): Task[Unit] = {
    if (channel == null) {
      channel = getChannel()
    }
    if (channel.isOpen) {
      def scheduleCallback(resume: (Task.Value[Unit] => Unit)): Unit = {
        channel.getCloseSetter.set(new ChannelListener[StreamSourceChannel] {
          override def handleEvent(channel: StreamSourceChannel): Unit = {
            if (channel.isOpen) {
              scheduleCallback(resume)
            } else {
              resume(Task.Value.Unit)
            }
          }
        })
      }
      Task.Suspend { resume: (Task.Value[Unit] => Unit) => scheduleCallback(resume) }
    } else {
      Task.Value.Unit
    }
  }
}
