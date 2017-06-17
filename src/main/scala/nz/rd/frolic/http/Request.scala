package nz.rd.frolic.http

import nz.rd.frolic.async.trickle.Trickle

trait Request extends HttpMessage {
  def method: String
  def uri: String
  def path: String
  def entity: Trickle[Byte]
}