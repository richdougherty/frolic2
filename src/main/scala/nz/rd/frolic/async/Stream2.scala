package nz.rd.frolic.async

import java.util

import nz.rd.frolic.async.Stream2.NonEmptyPiece

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait Stream2[+A]

object Stream2 {
  sealed trait Piece[+A] extends Stream2[A]
  final case object Empty extends Piece[Nothing]
  sealed trait NonEmptyPiece[+A] extends Piece[A]
  final case class Element[+A](value: A) extends NonEmptyPiece[A]
  trait Block[+A] extends NonEmptyPiece[A] {
    def toSeq: immutable.Seq[A]
  }
  object Block {
    case class Seq[B](seq: immutable.Seq[B]) extends Block[B] {
      override def toSeq = seq
    }
    trait Buffer[B] extends Block[B] {
      def buffer[Buf <: java.nio.Buffer](implicit bufferType: BufferType[B,Buf]): Buf
    }
  }
  final case class Concat[+A](left: Stream2[A], right: Stream2[A]) extends Stream2[A]
  final case class Computed[+A](task: Task[Stream2[A]]) extends Stream2[A]
  final case class Error(cause: Throwable) extends Stream2[Nothing]
}


sealed trait Read[+A]

object Read {
  trait Available[+A] extends Read[A] {
    def piece: Stream2.NonEmptyPiece[A]
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
      case Stream2.Empty::tail => read(tail)
      case (p: Stream2.NonEmptyPiece[A])::tail =>
        new Available[A] {
          override def piece: NonEmptyPiece[A] = p
          override def next: Read[A] = nonRecRead(tail)
        }
      case Stream2.Computed(task)::tail =>
        new Computed[A] {
          override def next(): Task[Read[A]] = task.flatMap { str: Stream2[A] =>
            val nextRead: Read[A] = nonRecRead(str::tail)
            nextRead match {
              case c: Computed[A] => c.next()
              case r => Task.Value(r)
            }
          }
        }
      case Stream2.Concat(left, right)::tail => read(left::right::tail)
    }
    read(str::Nil)
  }
}

trait Writer2[-A] {
  def write(piece: Stream2.Piece[A]): Task[Unit]
  def close(): Task[Unit]
}