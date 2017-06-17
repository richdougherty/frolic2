package nz.rd.frolic.async.trickle

import nz.rd.frolic.async.trickle.Trickle.{Block, Concat, Empty}

import scala.collection.immutable

case class SeqBlock[A](seq: immutable.Seq[A]) extends Block[A] {
  override def toSeq = seq
  override def size: Int = seq.size
  override def map[B](f: A => B): Trickle[B] = SeqBlock(seq.map(f))
  override def flatMap[B](f: A => Trickle[B]): Trickle[B] = {
    // TODO: Optimise performance: no need to concat Empty, iterate in reverse order if more efficient, etc
    seq.foldRight[Trickle[B]](Empty) {
      case (element, mappedTail) => Concat(f(element), mappedTail)
    }
  }
}
