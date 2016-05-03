package nz.rd.frolic.entity

trait MessageRewriter {
  def rewrite[A,B](msg: Message[A,B], arg: A, entityTransaction: EntityTransaction, result: Option[B]): Option[B]
}
