package nz.rd.frolic.backend.undertow

import java.io.IOException

import io.undertow.Undertow
import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.{HttpHandler, HttpServerExchange}
import nz.rd.frolic.async.{FunctionalInterpreter, Task, _}
import nz.rd.frolic.http.{Request, Response}

object UndertowBackend {

  def flatIoTask[A](f: IoCallback => Task[A]): Task[A] = {
    Task.Suspend { resume: (Task.Result[Unit] => Unit) =>
      val ioCallback = new IoCallback {
        override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit =
          resume(Task.Value.Unit)
        override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit =
          resume(Task.Throw(exception))
      }
      f(ioCallback)
    }
  }
  def ioTask[A](f: IoCallback => A): Task[A] = {
    flatIoTask { c: IoCallback => Task.Value(f(c)) }
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
        val t: Task[Unit] = Task.Sequence(Task.Value(exchange), f).sequence { result =>
          exchange.endExchange()
          result match {
            case Task.Throw(cause) =>
              System.err.println("Failure handling request")
              cause.printStackTrace()
            case _ => ()
          }
          Task.Value.Unit
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
