package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import io.undertow.io.Sender
import nz.rd.frolic.async.{Task, Writer, Writer2}

class SenderTasks(sender: Sender) {
//  def send(buffer: ByteBuffer): Task[Unit] = sendTask(sender.send(buffer, _))
//  def send(buffer: Array[ByteBuffer], callback: IoCallback)
  def send(data: String): Task[Unit] = UndertowBackend.ioTask(sender.send(data, _))
  def writeToSender(writer: Writer2[Byte]): Task[Unit] = {
    val buf = ByteBuffer.allocate(8 * 1024)
    for {
      _ <- UndertowBackend.ioTask(sender.send(buf, _))
      _ <- writer.write()
      _ <- send(writer)
    } yield ()

    //    writer match {
//      case Writer.WriteElement(element, f) =>
//        val bb = ByteBuffer.allocate(1)
//        //bb.put(element)
//        val sendTask: Task[Unit] =
//        Task.Sequence(sendTask, f)
//      case Writer.WriteChunk(chunk, f) =>
//        val bb = ByteBuffer.wrap(chunk.toArray[Byte])
//        val sendTask = UndertowBackend.ioTask(sender.send(bb, _))
//        Task.Sequence(sendTask, f)
//      case Writer.Close(f) =>
//        val closeTask = UndertowBackend.ioTask(sender.close(_))
//        Task.Sequence(closeTask, f)
//      case Writer.Error(cause: Throwable, f) =>
//        val closeTask = UndertowBackend.ioTask(sender.close(_))
//        Task.Sequence(closeTask, f)
//    }
  }

  //  def send(data: String, charset: Charset, callback: IoCallback)
  def close(): Task[Unit] = UndertowBackend.ioTask(sender.close(_))

  def asyncSendAndEnd(data: String): Unit = sender.send(data)
  def asyncClose(): Unit = sender.close()
}
