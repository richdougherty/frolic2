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
      exchange.getResponseSender.send(s"Hello world from thread: ${threadName}. ", ioContinuationCallback(
        Continuation.f { v: (HttpServerExchange, Sender) =>
          exchange.getResponseSender.send(s"Hello again from thread: ${threadName}.")
          Task.Value.Unit
        }
      ))
      Task.Value.Unit
    })
  }

  def ioContinuationCallback(k: (HttpServerExchange, Sender) --> Unit): IoCallback = {
    new IoCallback {
      override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit =
        onResult(Task.Value((exchange, sender)))
      override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit =
        onResult(Task.Throw(exception))
      private def onResult(v: Task.Result[(HttpServerExchange, Sender)]): Unit = {
        Task.run(Task.Sequence(v, k))
      }
    }
  }
}