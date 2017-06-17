package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import io.undertow.io.Sender
import nz.rd.frolic.async._
import nz.rd.frolic.async.trickle.{ByteBlock, Read, Trickle}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

final class SenderTasks(sender: Sender) {

  def send(data: String): Task[Unit] = UndertowBackend.ioSuspend(sender.send(data, _))
  def send(buffer: ByteBuffer): Task[Unit] = UndertowBackend.ioSuspend(sender.send(buffer, _))
  def send(buffers: Array[ByteBuffer]): Task[Unit] = UndertowBackend.ioSuspend(sender.send(buffers, _))

  def send(stream: Trickle[Byte]): Task[Unit] = send(Read.fromStream(stream))
  def send(read: Read[Byte]): Task[Unit] = {

    // TODO: Improve perf - lazy buffer allocation, alloc fewer buffers, send if total buffer size is too big, etc
    val buffers = ArrayBuffer[ByteBuffer]()

    def sendBuffers(): Task[Unit] = {
      println("Sending any buffers")
      if (buffers.isEmpty) Task.Unit else send(buffers.toArray)
    }

    @tailrec
    def sendRead(read: Read[Byte]): Task[Unit] = read match {
      case Read.Done =>
        println("Read is Done")
        sendBuffers()
      case Read.Error(cause) =>
        println("Read is Error")
        sendBuffers().thenTask(Task.Failure(cause))
      case available: Read.Available[Byte] =>
        println(s"Read is Available ($available)")
        available.piece match {
          case trickle.Trickle.Element(e) =>
            val buf = ByteBuffer.allocate(1)
            buf.put(e)
            buffers += buf
          case b: ByteBlock =>
            buffers += b.readOnlyBuffer
          case b: trickle.Trickle.Block[Byte] =>
            buffers += ByteBuffer.wrap(b.toSeq.toArray)
        }
        sendRead(available.next)
      case computed: Read.Computed[Byte] =>
        println(s"Read needs Computing")
        sendBuffers().flatThen(computed.next().flatMap(nonRecSendRead))
    }

    def nonRecSendRead(read: Read[Byte]): Task[Unit] = sendRead(read)

    sendRead(read)
  }

  def close(): Task[Unit] = UndertowBackend.ioSuspend(sender.close(_))

  def asyncSendAndEnd(data: String): Unit = sender.send(data)
  def asyncClose(): Unit = sender.close()
}
