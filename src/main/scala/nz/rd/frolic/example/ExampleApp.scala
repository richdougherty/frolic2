package nz.rd.frolic.example

import java.nio.ByteBuffer

import nz.rd.frolic.Frolic
import nz.rd.frolic.async.{Read, Stream2, Task}
import nz.rd.frolic.http.{Request, Response}

import scala.annotation.tailrec

object ExampleApp {
  def main(args: Array[String]): Unit = {
    Frolic.start { request: Request =>
      if (request.path == "/plaintext") {
        val response = new Response {
          override val statusCode: Int = 200
          override val body: Stream2[Byte] = Stream2.Block.ByteBlock(ByteBuffer.wrap("Hello world".getBytes("utf-8")))
        }
        Task.Success(response)
      } else {

        def readStreamSize(stream: Stream2[Byte]): Task[Int] = {
          println("Starting reading stream size")
          @tailrec
          def readLoop(acc: Int, read: Read[Byte]): Task[Int] = read match {
            case Read.Done => {
              println(s"Finished reading stream size: $acc")
              Task.Success(acc)
            }
            case Read.Error(t) => Task.Failure(t)
            case Read.Available(piece, nextRead) => readLoop(acc + piece.size, nextRead)
            case Read.Computed(readTask) => readTask.flatMap(readLoopNonTailRec(acc, _))
          }
          def readLoopNonTailRec(acc: Int, read: Read[Byte]): Task[Int] = readLoop(acc, read)

          readLoop(0, Read.fromStream(stream))
        }

        readStreamSize(request.entity).map { streamSize: Int =>
          new Response {
            override val statusCode: Int = 200
            override val body: Stream2[Byte] = {

              def countdown(n: Int): Stream2[Byte] = {
                if (n < 0) Stream2.Empty else {
                  Stream2.Computed(Task.Eval(Stream2.Concat(Stream2.Block.SeqBlock(s"$n\n".getBytes("utf-8").toVector), countdown(n - 1))))
                }
              }

              Stream2.Block.ByteBlock(s"Hello from thread: ${Thread.currentThread.getName}.\n") ++
                  Stream2.Block.ByteBlock(s"Read $streamSize bytes from request channel.\n") ++
                  Stream2.Block.ByteBlock(s"Hello from thread: ${Thread.currentThread.getName}.\n") ++
                  countdown(10) ++
                  Stream2.Block.ByteBlock(s"Hello again from thread: ${Thread.currentThread.getName}.\n")
            }
          }
        }

      }
    }
  }
}