package nz.rd.frolic.async

import java.nio.ByteBuffer
import java.nio.charset.Charset

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait Stream2[+A] {
  def map[B](f: A => B): Stream2[B]
  def flatMap[B](f: A => Stream2[B]): Stream2[B]
  def ++[B >: A](that: Stream2[B]): Stream2[B] = Stream2.Concat(this, that)
}

object Stream2 {
  final case object Empty extends Stream2[Nothing] {
    override def map[B](f: Nothing => B): Stream2[B] = Empty
    override def flatMap[B](f: Nothing => Stream2[B]): Stream2[B] = Empty
  }
  sealed trait Piece[+A] extends Stream2[A] {
    def size: Int
  }
  final case class Element[+A](value: A) extends Piece[A] {
    override def size: Int = 1
    override def map[B](f: A => B): Stream2[B] = Element(f(value))
    override def flatMap[B](f: A => Stream2[B]): Stream2[B] = f(value)
  }
  trait Block[+A] extends Piece[A] {
    def toSeq: immutable.Seq[A]
  }
  object Block {

    case class SeqBlock[A](seq: immutable.Seq[A]) extends Block[A] {
      override def toSeq = seq

      override def size: Int = seq.size

      override def map[B](f: A => B): Stream2[B] = SeqBlock(seq.map(f))

      override def flatMap[B](f: A => Stream2[B]): Stream2[B] = {
        // TODO: Optimise performance: no need to concat Empty, iterate in reverse order if more efficient, etc
        seq.foldRight[Stream2[B]](Empty) {
          case (element, mappedTail) => Concat(f(element), mappedTail)
        }
      }
    }

    class ByteBlock private[ByteBlock](byteBuffer: ByteBuffer) extends Block[Byte] {
      def readOnlyBuffer: ByteBuffer = byteBuffer.asReadOnlyBuffer()

      override def toSeq: immutable.Seq[Byte] = {
        val builder = Vector.newBuilder[Byte]
        val bb: ByteBuffer = byteBuffer.asReadOnlyBuffer()
        while (bb.remaining > 0) {
          val b: Byte = bb.get()
          builder += b
        }
        builder.result()
      }

      override def size: Int = byteBuffer.limit()

      override def map[B](f: Byte => B): Stream2[B] = SeqBlock(toSeq.map(f))

      override def flatMap[B](f: Byte => Stream2[B]): Stream2[B] = SeqBlock(toSeq).flatMap(f)

      override def toString: String = s"ByteBlock(size: $size)"
    }

    object ByteBlock {
      def apply(bb: ByteBuffer): ByteBlock = {
        // TODO: Checking the position is a good idea for safety in common cases, but need to add expert API for non-zero positions too.
        assert(bb.position == 0, s"ByteBuffer position was ${bb.position}. Please flip ByteBuffer for reading before creating ByteBlock.")
        new ByteBlock(bb.asReadOnlyBuffer()) // PERF: Using a read-only buffer is defensive, but has a slight performance hit
      }
      def apply(arr: Array[Byte]): ByteBlock = apply(ByteBuffer.wrap(arr))

      private val utf8 = Charset.forName("utf-8")
      /** Convert a UTF-8 string to a ByteBlock */
      def apply(str: String): ByteBlock = apply(str.getBytes(utf8))

      def unapply(byteBlock: ByteBlock): Option[ByteBuffer] = {
        Some(byteBlock.readOnlyBuffer)
      }
    }
  }

  final case class Concat[+A](left: Stream2[A], right: Stream2[A]) extends Stream2[A] {
    override def map[B](f: A => B): Stream2[B] = Concat(left.map(f), right.map(f))
    override def flatMap[B](f: A => Stream2[B]): Stream2[B] = Concat(left.flatMap(f), right.flatMap(f))
  }

  // TODO: Instead of just returning a Task, make this a function that takes an ReadRequest, e.g.
  // ReadRequest.Size(int), ReadRequest.Array, ReadRequest.ByteBuffer(buf), ReadRequest.Unspecified
  // This would make the API fit better with common stream APIs, which provide a variety of read methods.
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
  object Available {
    def unapply[A](available: Available[A]): Option[(Stream2.Piece[A], Read[A])] = Some((available.piece, available.next))
  }
  trait Computed[+A] extends Read[A] {
    def next(): Task[Read[A]]
  }
  object Computed {
    def unapply[A](computed: Computed[A]): Option[Task[Read[A]]] = Some(computed.next())
  }
  case object Done extends Read[Nothing]
  case class Error(cause: Throwable) extends Read[Nothing]

  def fromStream[A](str: Stream2[A]): Read[A] = {
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