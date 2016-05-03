package nz.rd.frolic.entity

trait MessageHandler {
  def handle[A,B](msg: Message[A,B], arg: A, tx: EntityTransaction): Option[B]
}

object MessageHandler {
  def singleMessage[A,B](msg: Message[A,B])(f: (A, EntityTransaction) => B): MessageHandler = new MessageHandler {
    override def handle[A1,B1](m: Message[A1,B1], arg: A1, tx: EntityTransaction): Option[B1] = {
      m match {
        case `msg` => Some(f(arg.asInstanceOf[A], tx).asInstanceOf[B1])
        case _ => None
      }
    }
    override def toString: String = s"MessageHandler.singleMessage($msg)"
  }
}

//class SingleMessageHandler[A,B](msg: Message[A,B]) extends MessageHandler {
//  protected def handle(arg: A, tx: EntityTransaction): A
//  override def handle[A](m: Message[A,B], arg, A, tx: EntityTransaction): Option[B] = {
//    msg match {
//      case `msg` => Some(f(tx).asInstanceOf[A])
//      case _ => None
//    }
//  }
//  override def toString: String = s"MessageHandler.singleMessage($msg)"
//
//}