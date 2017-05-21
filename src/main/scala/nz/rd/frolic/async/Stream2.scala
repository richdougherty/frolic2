package nz.rd.frolic.async

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait Stream2[+A] {
  def map[B](f: A => B): Stream2[B]
  def flatMap[B](f: A => Stream2[B]): Stream2[B]
}

object Stream2 {
  final case object Empty extends Stream2[Nothing] {
    override def map[B](f: Nothing => B): Stream2[B] = Empty
    override def flatMap[B](f: Nothing => Stream2[B]): Stream2[B] = Empty
  }
  sealed trait Piece[+A] extends Stream2[A]
  final case class Element[+A](value: A) extends Piece[A] {
    override def map[B](f: A => B): Stream2[B] = Element(f(value))
    override def flatMap[B](f: A => Stream2[B]): Stream2[B] = f(value)
  }
  trait Block[+A] extends Piece[A] {
    def toSeq: immutable.Seq[A]
  }
  object Block {
    case class Seq[A](seq: immutable.Seq[A]) extends Block[A] {
      override def toSeq = seq
      override def map[B](f: A => B): Stream2[B] = Seq(seq.map(f))
      override def flatMap[B](f: A => Stream2[B]): Stream2[B] = {
        // TODO: Optimise performance: no need to concat Empty, iterate in reverse order if more efficient, etc
        seq.foldRight[Stream2[B]](Empty) {
          case (element, mappedTail) => Concat(f(element), mappedTail)
        }
      }
    }
    case class ByteBuffer(val buffer: java.nio.ByteBuffer) extends Block[Byte] {
      override def toSeq: immutable.Seq[Byte] = {
        val builder = Vector.newBuilder[Byte]
        buffer.rewind()
        while (buffer.remaining > 0) {
          val b: Byte = buffer.get()
          builder += b
        }
        builder.result()
      }
      override def map[B](f: Byte => B): Stream2[B] = Seq(toSeq.map(f))
      override def flatMap[B](f: Byte => Stream2[B]): Stream2[B] = Seq(toSeq).flatMap(f)
    }
  }
  final case class Concat[+A](left: Stream2[A], right: Stream2[A]) extends Stream2[A] {
    override def map[B](f: A => B): Stream2[B] = Concat(left.map(f), right.map(f))
    override def flatMap[B](f: A => Stream2[B]): Stream2[B] = Concat(left.flatMap(f), right.flatMap(f))
  }
  final case class Computed[+A](task: Task[Stream2[A]]) extends Stream2[A] {
    override def map[B](f: A => B): Stream2[B] = Computed(task.map((s: Stream2[A]) => s.map(f)))
    override def flatMap[B](f: A => Stream2[B]): Stream2[B] = Computed(task.map((s: Stream2[A]) => s.flatMap(f)))
  }
  final case class Error(cause: Throwable) extends Stream2[Nothing] {
    override def map[B](f: Nothing => B): Stream2[B] = this
    override def flatMap[B](f: Nothing => Stream2[B]): Stream2[B] = this
  }
}


sealed trait Read[+A]

object Read {
  trait Available[+A] extends Read[A] {
    def piece: Stream2.Piece[A]
    def next: Read[A]
  }
  trait Computed[+A] extends Read[A] {
    def next(): Task[Read[A]]
  }
  case object Done extends Read[Nothing]
  case class Error(cause: Throwable) extends Read[Nothing]

  def stream[A](str: Stream2[A]): Read[A] = {
    // Avoid tailrec warnings
    def nonRecRead(stack: List[Stream2[A]]): Read[A] = read(stack)

    @tailrec
    def read(stack: List[Stream2[A]]): Read[A] = stack match {
      case Nil => Done
      case Stream2.Error(cause)::_ => Error(cause)
      case Stream2.Empty::tail=> read(tail)
      case (p: Stream2.Piece[A])::tail =>
        new Available[A] {
          override def piece: Stream2.Piece[A] = p
          override def next: Read[A] = nonRecRead(tail)
        }
      case Stream2.Concat(left, right)::tail => read(left::right::tail)
      case Stream2.Computed(task)::tail =>
        Task.simplify(task) match {
          case Task.Success(streamValue) => read(streamValue::tail)
          case Task.Failure(cause) => Error(cause)
          case simplifiedTask =>
            new Computed[A] {
              override def next(): Task[Read[A]] = simplifiedTask.flatMap { str: Stream2[A] =>
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

trait Writer2[-A] {
  def write(piece: Stream2.Piece[A]): Task[Unit]
  def close(): Task[Unit]
}