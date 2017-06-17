package nz.rd.frolic.http

import nz.rd.frolic.async.{Stream2, Task}

trait Response extends HttpMessage {
  def statusCode: Int
  def body: Stream2[Byte]
  def onDone: Task[Unit] = Task.Success.Unit
}