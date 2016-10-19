package nz.rd.frolic.example

import java.nio.ByteBuffer

import nz.rd.frolic.Frolic
import nz.rd.frolic.async.Task
import nz.rd.frolic.backend.undertow.{SenderTasks, UndertowBackend}
import nz.rd.frolic.http.{Request, Response}

object ExampleApp {
  def main(args: Array[String]): Unit = {
    Frolic.start { request: Request =>
      def threadName(): String = Thread.currentThread.getName

      val buffer = ByteBuffer.allocate(128)
      val response = new Response {
        override def statusCode: Int = 200
        override def send: (SenderTasks) => Task[Unit] = { sender: SenderTasks =>
          for {
            _ <- sender.send(s"Hello from thread: ${threadName}. ")
            bytesRead <- request.entityChannel.read(buffer)
            _ <- sender.send(s"Read $bytesRead bytes from request channel. ")
            _ <- sender.send(s"Hello again from thread: ${threadName}.")
          } yield ()
        }
      }
      Task.Value(response)
    }
  }
}