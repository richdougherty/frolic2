package nz.rd.frolic.example

import java.io.IOException

import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.HttpServerExchange
import io.undertow.util.Headers
import nz.rd.frolic.Frolic
import nz.rd.frolic.async.{-->, Continuation, Task}

object ExampleApp {
  def main(args: Array[String]): Unit = {
    Frolic.start(Continuation.f { exchange: HttpServerExchange =>
      exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "text/plain")
      def threadName(): String = Thread.currentThread.getName
      Task.Suspend[Unit, Unit] { resume: (Task.Result[Unit] => Unit) =>
        exchange.getResponseSender.send(s"Hello world from thread: ${threadName}. ", new IoCallback {
          override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit = resume(Task.Value(()))
          override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit = resume(Task.Throw(exception))
        })
      }.flatMap[Unit] { _ =>
        exchange.getResponseSender.send(s"Hello again from thread: ${threadName}.")
        Task.Value.Unit
      }
    })
  }
}