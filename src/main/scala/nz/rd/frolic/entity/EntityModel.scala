package nz.rd.frolic.entity

import nz.rd.frolic.entity.impl.EntityModelImpl

trait EntityModel {
  def create: Entity
  def setName(name: String): EntityModel
  def addHandler[A](handler: MessageHandler): EntityModel
  def addRewriter[A](rewriter: MessageRewriter): EntityModel
  def handlersFor(msg: Message[_,_]): Seq[MessageHandler]
  def rewritersFor(msg: Message[_,_]): Seq[MessageRewriter]
}

object EntityModel {
  val empty: EntityModel = EntityModelImpl.empty
}