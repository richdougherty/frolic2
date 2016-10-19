package nz.rd.frolic.http

import nz.rd.frolic.backend.undertow.StreamSourceChannelTasks

trait Request extends HttpMessage {
  def method: String
  def uri: String
  def path: String
  def entityChannel: StreamSourceChannelTasks
}