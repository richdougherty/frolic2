package nz.rd.frolic.async.trickle

import nz.rd.frolic.async.Task

import scala.collection.immutable

/**
 * A trickle is a functional stream.
 *
 * @tparam A The elements contained by this trickle.
 */
sealed trait Trickle[+A] {
  def map[B](f: A => B): Trickle[B]
  def flatMap[B](f: A => Trickle[B]): Trickle[B]
  def ++[B >: A](that: Trickle[B]): Trickle[B] = Trickle.Concat(this, that)
}

object Trickle {
  final case object Empty extends Trickle[Nothing] {
    override def map[B](f: Nothing => B): Trickle[B] = Empty
    override def flatMap[B](f: Nothing => Trickle[B]): Trickle[B] = Empty
  }

  sealed trait Piece[+A] extends Trickle[A] {
    def size: Int
  }

  final case class Element[+A](value: A) extends Piece[A] {
    override def size: Int = 1
    override def map[B](f: A => B): Trickle[B] = Element(f(value))
    override def flatMap[B](f: A => Trickle[B]): Trickle[B] = f(value)
  }

  trait Block[+A] extends Piece[A] {
    def toSeq: immutable.Seq[A]
  }

  final case class Concat[+A](left: Trickle[A], right: Trickle[A]) extends Trickle[A] {
    override def map[B](f: A => B): Trickle[B] = Concat(left.map(f), right.map(f))
    override def flatMap[B](f: A => Trickle[B]): Trickle[B] = Concat(left.flatMap(f), right.flatMap(f))
  }

  // TODO: Instead of just returning a Task, make this a function that takes an ReadRequest, e.g.
  // ReadRequest.Size(int), ReadRequest.Array, ReadRequest.ByteBuffer(buf), ReadRequest.Unspecified
  // This would make the API fit better with common stream APIs, which provide a variety of read methods.
  final case class Computed[+A](task: Task[Trickle[A]]) extends Trickle[A] {
    override def map[B](f: A => B): Trickle[B] = Computed(task.map((s: Trickle[A]) => s.map(f)))
    override def flatMap[B](f: A => Trickle[B]): Trickle[B] = Computed(task.map((s: Trickle[A]) => s.flatMap(f)))
  }

  def compute[A](block: => Trickle[A]): Computed[A] = Computed(Task.eval(block))

  final case class Error(cause: Throwable) extends Trickle[Nothing] {
    override def map[B](f: Nothing => B): Trickle[B] = this
    override def flatMap[B](f: Nothing => Trickle[B]): Trickle[B] = this
  }
}