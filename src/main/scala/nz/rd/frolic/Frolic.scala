package nz.rd.frolic

import com.googlecode.lanterna.TextColor
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import io.opentracing.Tracer
import io.undertow.Undertow
import nz.rd.frolic.async.{Interpreter, Task}
import nz.rd.frolic.backend.undertow.UndertowBackend
import nz.rd.frolic.http.{Request, Response}

import scala.annotation.tailrec

object Frolic {

  def start(interpreter: Interpreter, tracer: Tracer)(f: Request => Task[Response]): Unit = {

    val terminal: Terminal = {
      val factory = new DefaultTerminalFactory()
      factory.setForceTextTerminal(true)
      factory.createTerminal()
    }

    val undertowBackend = new UndertowBackend(tracer)
    val undertowHandler = undertowBackend.handlerForFrolicModel(interpreter, tracer)(f)

    val port = 8000
    val server: Undertow = Undertow.builder()
        .addHttpListener(port, "localhost")
        .setHandler(undertowHandler)
        .build()
    server.start()
    terminal.setForegroundColor(TextColor.ANSI.CYAN)
    System.err.println(s"Listening on port $port. Press [Escape] to exit.")
    terminal.setForegroundColor(TextColor.ANSI.WHITE)
    @tailrec
    def waitForEscape(): Unit = {
      val keyStroke: KeyStroke = terminal.readInput()
      if (keyStroke.getKeyType == KeyType.Escape) {
        server.stop()
      } else {
        //println(s"Ignoring: $keyStroke")
        waitForEscape()
      }
    }
    waitForEscape()
  }
}


