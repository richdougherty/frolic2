package nz.rd.frolic

import nz.rd.frolic.async.Task
import nz.rd.frolic.backend.undertow.UndertowBackend
import nz.rd.frolic.http.{Request, Response}

object Frolic {
  def start(f: Request => Task[Response]): Unit = UndertowBackend.startWrapped(f)
}


