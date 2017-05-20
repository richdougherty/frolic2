package nz.rd.frolic.backend.undertow

import java.nio.ByteBuffer

import io.undertow.io.Sender
import nz.rd.frolic.async._

import scala.annotation.tailrec

class SenderTasks(sender: Sender) {
//  def send(buffer: ByteBuffer): Task[Unit] = sendTask(sender.send(buffer, _))
//  def send(buffer: Array[ByteBuffer], callback: IoCallback)
  def send(data: String): Task[Unit] = UndertowBackend.ioTask(sender.send(data, _))
  def writeToSender(stream: Stream2[Byte]): Task[Unit] = {

    var _buf: ByteBuffer = null

    def lazyBuffer(): ByteBuffer = {
      if (_buf == null) {
        _buf = ByteBuffer.allocate(8 * 1024)
      }
      _buf
    }

    def haveDataInBuffer: Boolean = _buf != null && _buf.capacity > 0

    def bufferFull: Boolean = _buf != null && _buf.remaining == 0

    def nonRecSendData(read: Read[Byte]): Task[Unit] = sendData(read)

    @tailrec
    def sendData(read: Read[Byte]): Task[Unit] = read match {
      case Read.Done =>
        if (haveDataInBuffer) {
          UndertowBackend.ioTask(sender.send(lazyBuffer(), _))
        } else Task.Value.Unit
      case Read.Error(cause) =>
        UndertowBackend.ioTask(sender.close(_)).flatMap(_ => Task.Throw(cause))
      case computed: Read.Computed[Byte] =>
        val sendComputed: Task[Unit] = computed.next().flatMap(nonRecSendData)
        if (haveDataInBuffer) {
          UndertowBackend.ioTask(sender.send(lazyBuffer(), _)).flatMap(_ => sendComputed)
        } else {
          sendComputed
        }
      case available: Read.Available[Byte] =>
        available.piece match {
          case Stream2.Element(e) =>
            assert(_buf.remaining > 0) // Should be true since buffer should have been sent if already full
            lazyBuffer().put(e)
            if (bufferFull) {
              UndertowBackend.ioTask(sender.send(lazyBuffer(), _)).map(_ => nonRecSendData(available.next))
            } else sendData(available.next)
          case b: Stream2.Block.Buffer[Byte] =>
            val blockBuf: ByteBuffer = b.buffer
            if (haveDataInBuffer) {
              // Send both buffers together
              UndertowBackend.ioTask(sender.send(Array(_buf, blockBuf), _)).map(_ => nonRecSendData(available.next))
            } else {
              // Just send the block buffer
              UndertowBackend.ioTask(sender.send(blockBuf, _)).map(_ => nonRecSendData(available.next))
            }
          case b: Stream2.Block[Byte] =>
            val itr: Iterator[Byte] = b.toSeq.iterator
            val buf: ByteBuffer = lazyBuffer()

            def bufferAndSendBytes(): Task[Unit] = {
              @tailrec
              def bufferAndSendBytesInner(): Task[Unit] = {
                if (itr.hasNext) {
                  val b = itr.next()
                  buf.put(b)
                  if (bufferFull) {
                    UndertowBackend.ioTask(sender.send(buf, _)).map(_ => bufferAndSendBytes())
                  } else bufferAndSendBytesInner() // Copy more bytes
                } else nonRecSendData(available.next)
              }

              bufferAndSendBytesInner()
            }

            bufferAndSendBytes()
        }
    }

    sendData(Read.stream(stream))
  }

  def close(): Task[Unit] = UndertowBackend.ioTask(sender.close(_))

  def asyncSendAndEnd(data: String): Unit = sender.send(data)
  def asyncClose(): Unit = sender.close()
}
