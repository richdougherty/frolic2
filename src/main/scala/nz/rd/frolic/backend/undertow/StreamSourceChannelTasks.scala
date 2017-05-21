package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import nz.rd.frolic.async.{Continuation, Task}
import org.xnio.ChannelListener
import org.xnio.channels.StreamSourceChannel

class StreamSourceChannelTasks(channel: StreamSourceChannel) {
  private def readTask[A](f: StreamSourceChannel => A, needsCallback: A => Boolean): Task[A] = {
    val result: A = f(channel)
    if (needsCallback(result)) {
      def scheduleCallback(k: Continuation[A]): Unit = {
        channel.getReadSetter.set(new ChannelListener[StreamSourceChannel] {
          override def handleEvent(channel: StreamSourceChannel): Unit = {
            channel.suspendReads() // PERF: Can we avoid suspending reads?
            val result: A = f(channel)
            if (needsCallback(result)) {
              scheduleCallback(k)
            } else {
              k.resume(result)
            }
          }
        })
      }
      Task.Suspend { k: Continuation[A] => scheduleCallback(k) }
    } else {
      Task.Success(result)
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
    if (channel.isOpen) {
      def scheduleCallback(k: Continuation[Unit]): Unit = {
        channel.getCloseSetter.set(new ChannelListener[StreamSourceChannel] {
          override def handleEvent(channel: StreamSourceChannel): Unit = {
            if (channel.isOpen) {
              scheduleCallback(k)
            } else {
              k.resume(())
            }
          }
        })
      }
      Task.Suspend { k: Continuation[Unit] => scheduleCallback(k) }
    } else {
      Task.Success.Unit
    }
  }
}
