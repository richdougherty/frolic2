package nz.rd.frolic.async.trickle

import java.nio.ByteBuffer
import java.nio.charset.Charset

import nz.rd.frolic.async.trickle.Trickle.Block

import scala.collection.immutable

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
  override def map[B](f: Byte => B): Trickle[B] = SeqBlock(toSeq.map(f))
  override def flatMap[B](f: Byte => Trickle[B]): Trickle[B] = SeqBlock(toSeq).flatMap(f)
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
