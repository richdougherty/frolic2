package nz.rd.frolic.async.trickle

import nz.rd.frolic.async.Task

import scala.annotation.tailrec

/**
 * The result of a read operation on a [[Trickle]].
 */
sealed trait Read[+A]

object Read {
  trait Available[+A] extends Read[A] {
    def piece: Trickle.Piece[A]
    def next: Read[A]
  }
  object Available {
    def unapply[A](available: Available[A]): Option[(Trickle.Piece[A], Read[A])] = Some((available.piece, available.next))
  }
  trait Computed[+A] extends Read[A] {
    def next(): Task[Read[A]]
  }
  object Computed {
    def unapply[A](computed: Computed[A]): Option[Task[Read[A]]] = Some(computed.next())
  }
  case object Done extends Read[Nothing]
  case class Error(cause: Throwable) extends Read[Nothing]

  def fromStream[A](str: Trickle[A]): Read[A] = {
    // Avoid tailrec warnings
    def nonRecRead(stack: List[Trickle[A]]): Read[A] = read(stack)

    @tailrec
    def read(stack: List[Trickle[A]]): Read[A] = stack match {
      case Nil => Done
      case Trickle.Error(cause)::_ => Error(cause)
      case Trickle.Empty::tail=> read(tail)
      case (p: Trickle.Piece[A])::tail =>
        new Available[A] {
          override def piece: Trickle.Piece[A] = p
          override def next: Read[A] = nonRecRead(tail)
        }
      case Trickle.Concat(left, right)::tail => read(left::right::tail)
      case Trickle.Computed(task)::tail =>
        Task.simplify(task) match {
          case Task.Success(streamValue) => read(streamValue::tail)
          case Task.Failure(cause) => Error(cause)
          case simplifiedTask =>
            new Computed[A] {
              override def next(): Task[Read[A]] = simplifiedTask.flatMap { str: Trickle[A] =>
                val nextRead: Read[A] = nonRecRead(str::tail)
                nextRead match {
                  case c: Computed[A] => c.next()
                  case r => Task.Success(r)
                }
              }
            }
        }
    }
    read(str::Nil)
  }
}