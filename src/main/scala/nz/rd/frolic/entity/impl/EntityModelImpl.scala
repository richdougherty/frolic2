package nz.rd.frolic.entity.impl

import nz.rd.frolic.entity._

private class EntityModelImpl(
                       name: Option[String],
                       handlers: Seq[MessageHandler],
                       rewriters: Seq[MessageRewriter]
                     ) extends EntityModel {

  override def setName(name: String): EntityModel =  {
    new EntityModelImpl(Some(name), handlers, rewriters)
  }

  override def addHandler[A](handler: MessageHandler): EntityModel = {
    new EntityModelImpl(name, handler +: handlers, rewriters)
  }

  override def addRewriter[A](rewriter: MessageRewriter): EntityModel = {
    new EntityModelImpl(name, handlers, rewriter +: rewriters)
  }

  override def handlersFor(msg: Message[_,_]): Seq[MessageHandler] = handlers

  override def rewritersFor(msg: Message[_,_]): Seq[MessageRewriter] = rewriters

  override def create: Entity = new EntityImpl(this, Map.empty)

  override def toString: String = s"EntityModel($name, $handlers, $rewriters)"
}

object EntityModelImpl {
  def empty: EntityModel = new EntityModelImpl(
    name = None,
    handlers = Seq.empty,
    rewriters = Seq.empty
  )
}
