package nz.rd.frolic.http

import nz.rd.frolic.async.Stream2

trait Request extends HttpMessage {
  def method: String
  def uri: String
  def path: String
  def entity: Stream2[Byte]
}