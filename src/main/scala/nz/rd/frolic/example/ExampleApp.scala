package nz.rd.frolic.example

import java.nio.ByteBuffer

import brave.Tracing
import brave.opentracing.BraveTracer
import io.opentracing.mock.MockTracer
import io.opentracing.{NoopTracerFactory, Tracer}
import nz.rd.frolic.Frolic
import nz.rd.frolic.async.trickle.{ByteBlock, Read, SeqBlock, Trickle}
import nz.rd.frolic.async.{FunctionalInterpreter, Task}
import nz.rd.frolic.http.{Request, Response}
import nz.rd.frolic.integrations.opentracing.{OpenTracingInterpreterListener, SuspendableActiveSpanSource, SuspendableTracer}
import zipkin.reporter.AsyncReporter
import zipkin.reporter.okhttp3.OkHttpSender

import scala.annotation.tailrec

object ExampleApp {
  def main(args: Array[String]): Unit = {

//    val useJaeger = args.contains("tracer=jaeger")
    val useZipkin = args.contains("tracer=zipkin")
    val useMock = args.contains("tracer=zipkin")


    val tracer: SuspendableTracer = {
      val suspendableActiveSpanSource: SuspendableActiveSpanSource = new SuspendableActiveSpanSource
      val underlyingTracer: Tracer =
      //      if (useJaeger) {
      //      println("Using Jaeger for tracing")
      //      val conf = new com.uber.jaeger.Configuration(
      //        "FrolicExampleApp",
      //        new com.uber.jaeger.Configuration.SamplerConfiguration("const", 1),
      //        new com.uber.jaeger.Configuration.ReporterConfiguration(
      //          true,  // logSpans
      //          "localhost",
      //          5775,
      //          1000,   // flush interval in milliseconds
      //          10000)  // max buffered Spans
      //      )
      //      conf.getTracer()
      //    } else
        if (useZipkin) {
          //println("Using Zipkin for tracing")
          val sender = OkHttpSender.create("http://localhost:9411/api/v1/spans")
          val reporter = AsyncReporter.builder(sender).build()
          val braveTracing = Tracing.newBuilder()
              .localServiceName("ExampleApp")
              .reporter(reporter)
              .build()
          BraveTracer.newBuilder(braveTracing)
              .activeSpanSource(suspendableActiveSpanSource)
              .build()

        } else if (useMock) {
          //println("Using mock tracing")
          new MockTracer(suspendableActiveSpanSource)
        } else {
          NoopTracerFactory.create()
        }
      new SuspendableTracer(underlyingTracer, suspendableActiveSpanSource)
    }

    val interpreter = new FunctionalInterpreter(
      new OpenTracingInterpreterListener(tracer))

    Frolic.start(interpreter, tracer) { request: Request =>
      if (request.path == "/plaintext") {
        val response = new Response {
          override val statusCode: Int = 200
          override val body: Trickle[Byte] = ByteBlock(ByteBuffer.wrap("Hello world".getBytes("utf-8")))
        }
        Task.Success(response)
      } else {

        def readStreamSize(stream: Trickle[Byte]): Task[Int] = {
          //println("Starting reading stream size")
          @tailrec
          def readLoop(acc: Int, read: Read[Byte]): Task[Int] = read match {
            case Read.Done => {
              //println(s"Finished reading stream size: $acc")
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
            override val body: Trickle[Byte] = {

              def countdown(n: Int): Trickle[Byte] = {
                if (n < 0) Trickle.Empty else {
                  (Trickle.compute { SeqBlock(s"$n\n".getBytes("utf-8").toVector) }) ++ countdown(n - 1)
                }
              }

              ByteBlock(s"Hello from thread: ${Thread.currentThread.getName}.\n") ++
                  ByteBlock(s"Read $streamSize bytes from request channel.\n") ++
                  ByteBlock(s"Hello from thread: ${Thread.currentThread.getName}.\n") ++
                  countdown(10) ++
                  ByteBlock(s"Hello again from thread: ${Thread.currentThread.getName}.\n")
            }
          }
        }

      }
    }
  }
}