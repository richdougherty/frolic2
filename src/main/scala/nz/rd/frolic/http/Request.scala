package nz.rd.frolic.http

import nz.rd.frolic.entity.{Field, Entity}

class Request(entity: Entity) extends Message(entity) {

  override protected type NewInstance = Request

  override protected def newInstance(newEntity: Entity): Request = new Request(newEntity)

  def method: String = entity.sendRead(Field.Get(Request.method), ())
  def rawUri: String = entity.sendRead(Field.Get(Request.rawUri), ())
  def rawPath: String = entity.sendRead(Field.Get(Request.rawPath), ())
}

object Request {
  val method = Field[String]("method")
  val rawUri = Field[String]("rawUri")
  val rawPath = Field[String]("rawPath")
}
