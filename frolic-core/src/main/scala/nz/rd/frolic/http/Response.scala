package nz.rd.frolic.http

import nz.rd.frolic.async.Task
import nz.rd.frolic.async.trickle.Trickle

trait Response extends HttpMessage {
  def statusCode: Int
  def body: Trickle[Byte]
  def onDone: Task[Unit] = Task.Unit
}