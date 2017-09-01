package nz.rd.frolic

import io.opentracing.Tracer
import nz.rd.frolic.async.{Interpreter, Task}
import nz.rd.frolic.backend.undertow.UndertowBackend
import nz.rd.frolic.http.{Request, Response}

object Frolic {
  def start(interpreter: Interpreter, tracer: Tracer)(f: Request => Task[Response]): Unit = {
    val undertowBackend = new UndertowBackend(tracer)
    undertowBackend.startWrapped(interpreter, tracer)(f)
  }
}


