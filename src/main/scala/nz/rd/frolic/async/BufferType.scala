package nz.rd.frolic.async

import java.nio.{Buffer, ByteBuffer}

import scala.collection.immutable

trait BufferType[A,B <: Buffer] {
  def toSeq(buf: B): immutable.Seq[A]
  def toArray(buf: B): Array[A]
}
object BufferType {
  private class ConcreteBufferType[A,B] extends BufferType[A,B]
  implicit val byteBufferType: BufferType[Byte, ByteBuffer] = new ConcreteBufferType[Byte,ByteBuffer]
}
