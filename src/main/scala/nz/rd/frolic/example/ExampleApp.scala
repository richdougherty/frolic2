package nz.rd.frolic.example

import io.undertow.server.HttpServerExchange
import nz.rd.frolic.Frolic
import nz.rd.frolic.async.{Continuation, Task}

object ExampleApp {
  def main(args: Array[String]): Unit = {
    Frolic.start(Continuation.f { exchange: HttpServerExchange =>
      def threadName(): String = Thread.currentThread.getName
      for {
        _ <- Frolic.ioTask[Unit] { ioCallback =>
          exchange.getResponseSender.send(s"Hello world from thread: ${threadName}. ", ioCallback)
        }
        _ <- Task.Eval {
          exchange.getResponseSender.send(s"Hello again from thread: ${threadName}.")
        }
      } yield ()
    })
  }
}