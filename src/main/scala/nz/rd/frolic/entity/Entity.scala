package nz.rd.frolic.entity

trait Entity {
  def send[A,B](msg: Message[A,B], arg: A): (Entity, B)
  def sendRead[A,B](msg: Message[A,B], arg: A): B
  def sendWrite[A](msg: Message[A,_], arg: A): Entity

//
//  final def get[A](field: Field[A]): A = {
//    val tx = new EntityTransaction(data)
//    val result = tx.send(Get(field))
//    assert(!tx.dirty, "Get messages must not modify the transaction state")
//    data = tx.commit()
//    result
//  }
//  def set(msg: Message[Unit]): Entity

}