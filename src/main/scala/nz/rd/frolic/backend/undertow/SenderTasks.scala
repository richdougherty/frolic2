package nz.rd.frolic.backend.undertow

import io.undertow.io.Sender
import nz.rd.frolic.async.Task

class SenderTasks(sender: Sender) {
//  def send(buffer: ByteBuffer): Task[Unit] = sendTask(sender.send(buffer, _))
//  def send(buffer: Array[ByteBuffer], callback: IoCallback)
  def send(data: String): Task[Unit] = UndertowBackend.ioTask(sender.send(data, _))
  def send(writer: Writer[Byte]): Task[Unit] = writer match {
    case WriteElement(element, f) =>
      Task.Sequence(UndertowBackend.ioTask(sender.send(Array[Byte](element), _)), f)
    case WriteChunk(chunk, f) =>
    case Close(f) =>
    case Error(cause: Throwable) =>

  }

  //  def send(data: String, charset: Charset, callback: IoCallback)
  def close(): Task[Unit] = UndertowBackend.ioTask(sender.close(_))

  def asyncSendAndEnd(data: String): Unit = sender.send(data)
  def asyncClose(): Unit = sender.close()
}
