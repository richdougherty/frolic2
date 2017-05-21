package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import io.undertow.io.Sender
import nz.rd.frolic.async._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class SenderTasks(sender: Sender) {

  def send(data: String): Task[Unit] = UndertowBackend.ioSuspend(sender.send(data, _))
  def send(buffer: ByteBuffer): Task[Unit] = UndertowBackend.ioSuspend(sender.send(buffer, _))
  def send(buffers: Array[ByteBuffer]): Task[Unit] = UndertowBackend.ioSuspend(sender.send(buffers, _))

  def send(stream: Stream2[Byte]): Task[Unit] = {

    // TODO: Improve perf - lazy buffer allocation, alloc fewer buffers, send if total buffer size is too big, etc
    val buffers = ArrayBuffer[ByteBuffer]()

    def sendBuffers(): Task[Unit] = {
      if (buffers.isEmpty) Task.Success.Unit else send(buffers.toArray)
    }

    @tailrec
    def sendRead(read: Read[Byte]): Task[Unit] = read match {
      case Read.Done =>
        sendBuffers()
      case Read.Error(cause) =>
        sendBuffers().andThen(Task.Failure(cause))
      case available: Read.Available[Byte] =>
        available.piece match {
          case Stream2.Element(e) =>
            val buf = ByteBuffer.allocate(1)
            buf.put(e)
            buffers += buf
          case b: Stream2.Block.ByteBuffer =>
            buffers += b.buffer
          case b: Stream2.Block[Byte] =>
            buffers += ByteBuffer.wrap(b.toSeq.toArray)
        }
        sendRead(available.next)
      case computed: Read.Computed[Byte] =>
        sendBuffers().andThen(computed.next().flatMap(nonRecSendRead))
    }

    def nonRecSendRead(read: Read[Byte]): Task[Unit] = sendRead(read)

    sendRead(Read.stream(stream))
  }

  def close(): Task[Unit] = UndertowBackend.ioSuspend(sender.close(_))

  def asyncSendAndEnd(data: String): Unit = sender.send(data)
  def asyncClose(): Unit = sender.close()
}
