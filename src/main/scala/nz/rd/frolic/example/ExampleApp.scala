package nz.rd.frolic.example

import java.nio.ByteBuffer

import nz.rd.frolic.Frolic
import nz.rd.frolic.async.{Stream2, Task}
import nz.rd.frolic.backend.undertow.{SenderTasks, UndertowBackend}
import nz.rd.frolic.http.{Request, Response}

object ExampleApp {
  def main(args: Array[String]): Unit = {
    Frolic.start { request: Request =>
      if (request.path == "/plaintext") {
        val response = new Response {
          override def statusCode: Int = 200
          override def send: (SenderTasks) => Task[Unit] = { sender: SenderTasks =>
            sender.asyncSendAndEnd("Hello world")
            Task.Success.Unit
          }
        }
        Task.Success(response)
      } else {
        def threadName(): String = Thread.currentThread.getName

        val buffer = ByteBuffer.allocate(128)
        val response = new Response {
          override def statusCode: Int = 200

          def countdown(n: Int): Stream2[Byte] = {
            if (n < 0) Stream2.Empty else {
              Stream2.Computed(Task.Eval(Stream2.Concat(Stream2.Block.Seq(s"$n\n".getBytes("utf-8").toVector), countdown(n - 1))))
            }
          }

          override def send: (SenderTasks) => Task[Unit] = { sender: SenderTasks =>
            for {
              _ <- sender.send(s"Hello from thread: ${threadName}.\n")
              bytesRead <- request.entityChannel.read(buffer)
              _ <- sender.send(s"Read $bytesRead bytes from request channel.\n")
              _ <- sender.send(s"Hello again from thread: ${threadName}.\n")
              _ <- sender.send(countdown(10))
            } yield ()
          }
        }
        Task.Success(response)
      }
    }
  }
}