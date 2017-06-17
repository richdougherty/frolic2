package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import nz.rd.frolic.async.trickle.{ByteBlock, Trickle}
import nz.rd.frolic.async.{Continuation, Task}
import org.xnio.ChannelListener
import org.xnio.channels.StreamSourceChannel

class StreamSourceChannelTasks(channel: StreamSourceChannel) {
  private def readTask[A](readFromChannel: StreamSourceChannel => A, readAgainLater: A => Boolean): Task[A] = {
    val result: A = readFromChannel(channel)

    if (readAgainLater(result)) {

      def scheduleCallback(k: Continuation[A]): Unit = {
        channel.getReadSetter.set((channel: StreamSourceChannel) => {
          // PERF: Can we avoid suspending reads?
          // FIXME: Check that we can't get multiple reads - maybe use a counter to ensure this is only called once
          channel.suspendReads()
          val result: A = readFromChannel(channel)
          if (readAgainLater(result)) {
            scheduleCallback(k)
          } else {
            k.resume(result)
          }
        })
      }

      Task.suspend { k: Continuation[A] => scheduleCallback(k) }

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

  def stream(): Trickle[Byte] = {
    Trickle.Computed(Task.flatEval {
      val buf = ByteBuffer.allocate(256)
      read(buf).map { bytesRead: Int =>
        if (bytesRead == -1) {
          Trickle.Empty: Trickle[Byte] // The stream is done/empty
        } else {
          assert(bytesRead != 0) // Our read methods retry if there no bytes to read, so this shouldn't happen
          Trickle.Concat[Byte](ByteBlock(buf), stream())
        }
      }
    })
  }

  def close(): Unit = channel.close()
  def isOpen(): Boolean = channel.isOpen

  def waitForClose(): Task[Unit] = {
    if (channel.isOpen) {
      def scheduleCallback(k: Continuation[Unit]): Unit = {
        channel.getCloseSetter.set((channel: StreamSourceChannel) => {
          if (channel.isOpen) {
            scheduleCallback(k)
          } else {
            k.resume(())
          }
        })
      }
      Task.suspend { k: Continuation[Unit] => scheduleCallback(k) }
    } else {
      Task.Unit
    }
  }
}
