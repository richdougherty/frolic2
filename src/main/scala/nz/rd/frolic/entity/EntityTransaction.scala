package nz.rd.frolic.entity

trait EntityTransaction {
  def send[A,B](msg: Message[A,B], arg: A): B
  def get[A](slot: Slot[A]): A
  def set[A](slot: Slot[A], value: A): Unit
  def cache[A](slot: Slot[A], value: A): Unit
}