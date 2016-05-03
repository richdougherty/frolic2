package nz.rd.frolic.http

import nz.rd.frolic.entity.{Field, Entity}

abstract class Message(val entity: Entity) {

  protected type NewInstance

  protected def newInstance(newEntity: Entity): NewInstance

  final def mapEntity(f: Entity => Entity): NewInstance = newInstance(f(entity))

  def getRawHeaders(name: String): Seq[String] = entity.sendRead(Field.Get(Message.rawHeaders(name)), ())
  def getContent: String = entity.sendRead(Field.Get(Message.content), ())
  def setContent(content: String): NewInstance = newInstance(entity.sendWrite(Field.Set(Message.content), content))
}

object Message {
  def rawHeaders(name: String): Field[Seq[String]] = new Field[Seq[String]](Some("rawHeaders("+name+")"))
  val content: Field[String] = new Field[String](Some("content"))
}