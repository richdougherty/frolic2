package nz.rd.frolic.example

import java.nio.ByteBuffer

import io.undertow.server.HttpServerExchange
import nz.rd.frolic.async.{Continuation, Task}
import nz.rd.frolic.backend.undertow.{StreamSourceChannelTasks, UndertowBackend}

object ExampleApp {
  def main(args: Array[String]): Unit = {
    UndertowBackend.start(Continuation.f { exchange: HttpServerExchange =>
      val requestChannel = StreamSourceChannelTasks(exchange.getRequestChannel())
      def threadName(): String = Thread.currentThread.getName

      val buffer = ByteBuffer.allocate(128)

      for {
        _ <- UndertowBackend.ioTask[Unit] { ioCallback =>
          exchange.getResponseSender.send(s"Hello from thread: ${threadName}. ", ioCallback)
        }
        bytesRead <- requestChannel.read(buffer)
        _ <- UndertowBackend.ioTask[Unit] { ioCallback =>
          exchange.getResponseSender.send(s"Read $bytesRead bytes from request channel. ", ioCallback)
        }
        _ <- UndertowBackend.ioTask[Unit] { ioCallback =>
          exchange.getResponseSender.send(s"Hello again from thread: ${threadName}.", ioCallback)
        }
      } yield ()
    })
  }
}