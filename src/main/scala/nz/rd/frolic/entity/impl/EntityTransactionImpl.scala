package nz.rd.frolic.entity.impl

import nz.rd.frolic.entity.{EntityTransaction, Message, Slot}

class EntityTransactionImpl(entityModel: EntityModelImpl, initialData: Data) extends EntityTransaction {

  private var committed: Boolean = false
  private var dirtySetOccurred: Boolean = false
  private var data: Data = initialData

  private def checkNotCommitted(): Unit = {
    if (committed) throw new IllegalStateException("EntityTransaction has already been committed.")
  }

  override def send[A,B](msg: Message[A,B], arg: A): B = {
    checkNotCommitted()
    // PERF: Can optimize this using while loops
    val handleResult: Option[B] = entityModel.handlersFor(msg).foldLeft[Option[B]](None) {
      case (None, handler) => handler.handle(msg, arg, this)
      case (s@Some(_), _) => s
    }
    val rewriteResult = entityModel.rewritersFor(msg).foldLeft[Option[B]](handleResult) {
      case (r, rewriter) => rewriter.rewrite(msg, arg, this, r)
    }
    rewriteResult.getOrElse(throw new UnsupportedOperationException(s"Message $msg not handled by entity $entityModel"))
  }
  override def get[A](slot: Slot[A]): A = {
    checkNotCommitted()
    data.getOrElse(slot, null).asInstanceOf[A]
  }
  override def set[A](slot: Slot[A], value: A): Unit = {
    checkNotCommitted()
    dirtySetOccurred = true
    data = data.updated(slot, value)
  }
  override def cache[A](slot: Slot[A], value: A): Unit = {
    checkNotCommitted()
    data = data.updated(slot, value)
  }
  def dirty: Boolean = dirtySetOccurred

  def commit(): Data = {
    checkNotCommitted()
    committed = true
    data
  }
}