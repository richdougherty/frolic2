package nz.rd.frolic.backend.undertow

import java.io.IOException

import io.undertow.Undertow
import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.{HttpHandler, HttpServerExchange}
import nz.rd.frolic.async.{FunctionalInterpreter, Task, _}
import nz.rd.frolic.http.{Request, Response}

import scala.util.control.NonFatal

object UndertowBackend {

  /**
   * Creates a task that suspends the computation until an [[IoCallback]] is called.
   * The `IoCallback` is be provided to the given block of code when the task
   * is suspended. Calling the `IoCallback` will resume the suspended computation
   * by calling its [[Continuation]].
   *
   * For example:
   *
   * ```
   * ioSuspend { resume: IoCallback => sender.send("Hello world", resume) }
   * ```
   *
   * Or more succinctly:
   *
   * ```
   * ioSuspend(sender.send("Hello world", _))
   * ```
   */
  def ioSuspend(block: IoCallback => Unit): Task.Suspend[Unit] = {
    Task.Suspend { k: Continuation[Unit] =>
      val ioCallback = new IoCallback {
        override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit =
          k.resume(())
        override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit =
          k.resumeWithException(exception)
      }
      block(ioCallback)
    }
  }

  def startWrapped(f: Request => Task[Response]): Unit = {

    start { exchange: HttpServerExchange =>
      val request = new Request {
        override def method: String = exchange.getRequestMethod.toString
        override def uri: String = exchange.getRequestURI
        override def path: String = exchange.getRequestPath
        override lazy val entityChannel: StreamSourceChannelTasks =
          new StreamSourceChannelTasks(exchange.getRequestChannel())
      }
      val t: Task[Unit] = f(request).flatMap { response: Response =>
        exchange.setResponseCode(response.statusCode)
        val sender: Sender = exchange.getResponseSender()
        response.send(new SenderTasks(sender))
      }
      t
    }
  }

  def start(f: HttpServerExchange => Task[Unit]): Unit = {

    def httpHandler = new HttpHandler {

      override def handleRequest(exchange: HttpServerExchange): Unit = {
        exchange.dispatch()
        val t: Task[Unit] = Task.Success(exchange).flatMap(f).`catch` {
          case NonFatal(t) =>
            System.err.println("Failure handling request")
            t.printStackTrace()
        }.`finally` {
          exchange.endExchange()
        }
        new FunctionalInterpreter().run(t)
      }
    }

    val server: Undertow = Undertow.builder()
        .addHttpListener(8000, "localhost")
        .setHandler(httpHandler)
        .build()
    server.start()
  }
}
