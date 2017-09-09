package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import io.opentracing.{ActiveSpan, Tracer}
import io.undertow.io.Sender
import nz.rd.frolic.async._
import nz.rd.frolic.async.trickle.{ByteBlock, Read, Trickle}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

final class SenderTasks(tracer: Tracer, sender: Sender) {

  val undertowBackend = new UndertowBackend(tracer)

  def send(data: String): Task[Unit] = undertowBackend.ioSuspend("send String")(sender.send(data, _))
  def send(buffer: ByteBuffer): Task[Unit] = undertowBackend.ioSuspend("send ByteBuffer")(sender.send(buffer, _))
  def send(buffers: Array[ByteBuffer]): Task[Unit] = undertowBackend.ioSuspend("send Array")(sender.send(buffers, _))

  def send(stream: Trickle[Byte]): Task[Unit] = send(Read.fromStream(stream))
  def send(read: Read[Byte]): Task[Unit] = {

    val sendSpan: ActiveSpan = tracer.buildSpan("send Trickle Read").startActive()

    // TODO: Improve perf - lazy buffer allocation, alloc fewer buffers, send if total buffer size is too big, etc
    val buffers = ArrayBuffer[ByteBuffer]()

    def sendBuffers(): Task[Unit] = {
      if (buffers.isEmpty) {
        sendSpan.log("No buffers to send")
        Task.Unit
      } else {
        sendSpan.log("Sending buffers")
        send(buffers.toArray)
      }
    }

    @tailrec
    def sendRead(read: Read[Byte]): Task[Unit] = {
      //assert(tracer.activeSpan() == sendSpan, s"Expected span of $sendSpan, was ${tracer.activeSpan()}")
      read match {
        case Read.Done =>
          sendSpan.log("Read is Done")
          sendBuffers()
        case Read.Error(cause) =>
          sendSpan.log("Read is Error")
          sendBuffers().thenTask(Task.Failure(cause))
        case available: Read.Available[Byte] =>
          sendSpan.log(s"Read is Available ($available)")
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
          sendSpan.log(s"Read needs Computing")
          sendBuffers().flatThen(computed.next().flatMap(nonRecSendRead))
      }
    }

    def nonRecSendRead(read: Read[Byte]): Task[Unit] = sendRead(read)

    sendRead(read)
  }

  def close(): Task[Unit] = undertowBackend.ioSuspend("close")(sender.close(_))

  def asyncSendAndEnd(data: String): Unit = sender.send(data)
  def asyncClose(): Unit = sender.close()
}
