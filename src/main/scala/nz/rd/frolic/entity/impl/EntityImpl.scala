package nz.rd.frolic.entity.impl

import nz.rd.frolic.entity.{Entity, Message}

final class EntityImpl(entityModel: EntityModelImpl, private var data: Data) extends Entity {
  override def send[A,B](msg: Message[A,B], arg: A): (Entity, B) = {
    val tx = new EntityTransactionImpl(entityModel, data)
    val result = tx.send(msg, arg)
    val newData = tx.commit()
    (new EntityImpl(entityModel, newData), result)
  }
  override def sendRead[A,B](msg: Message[A,B], arg: A): B = {
    val tx = new EntityTransactionImpl(entityModel, data)
    val result = tx.send(msg, arg)
    assert(!tx.dirty, "Pure messages must not modify the entity")
    data = tx.commit()
    result
  }
  override def sendWrite[A](msg: Message[A,_], arg: A): Entity = {
    val tx = new EntityTransactionImpl(entityModel, data)
    tx.send(msg, arg)
    val newData = tx.commit()
    new EntityImpl(entityModel, newData)
  }
}