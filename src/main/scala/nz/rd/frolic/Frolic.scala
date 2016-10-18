package nz.rd.frolic

import io.undertow.Undertow
import io.undertow.server.{HttpHandler, HttpServerExchange}
import nz.rd.frolic.async.{-->, FunctionalInterpreter, Task}

object Frolic {

  def start(f: (HttpServerExchange) --> Unit): Unit = {

    def httpHandler = new HttpHandler {

      override def handleRequest(exchange: HttpServerExchange): Unit = {
        val t: Task[Unit] = Task.Sequence(Task.Value(exchange), f).andThen {
          case Task.Value(_) =>
            Task.Value.Unit
          case Task.Throw(cause) =>
            System.err.println("Failure handling request")
            cause.printStackTrace()
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


