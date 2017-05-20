package nz.rd.frolic.async

import java.nio.{Buffer, ByteBuffer}

import scala.collection.immutable

trait BufferType[A,B <: Buffer]

object BufferType {
  private class ConcreteBufferType[A,B <: Buffer] extends BufferType[A,B]
  implicit val byteBufferType: BufferType[Byte, ByteBuffer] = new ConcreteBufferType[Byte,ByteBuffer]
}
