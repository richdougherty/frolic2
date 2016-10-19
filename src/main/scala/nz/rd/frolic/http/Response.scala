package nz.rd.frolic.http

import nz.rd.frolic.async.Task
import nz.rd.frolic.backend.undertow.SenderTasks

trait Response extends HttpMessage {
  def statusCode: Int
  def send: SenderTasks => Task[Unit]
}