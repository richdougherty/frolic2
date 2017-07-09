package nz.rd.frolic.backend.undertow

import java.io.IOException
import java.util
import java.util.Map

import io.opentracing.propagation.{Format, TextMap}
import io.opentracing.{ActiveSpan, Tracer}
import io.undertow.Undertow
import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.{HttpHandler, HttpServerExchange}
import io.undertow.util.{HttpString, SameThreadExecutor}
import nz.rd.frolic.async.trickle.{Read, Trickle}
import nz.rd.frolic.async.{FunctionalInterpreter, Task, trickle, _}
import nz.rd.frolic.http.{Request, Response}
import nz.rd.frolic.integrations.opentracing.OpenTracingInterpreterListener

import scala.util.control.NonFatal

object UndertowBackend {

  /**
   * Creates a task that suspends the computation until an [[IoCallback]] is called.
   * The `IoCallback` is be provided to the given block of code when the task
   * is suspended. Calling the `IoCallback` will resume the suspended computation
   * by calling its [[Continuation]].
   *
   * For example:
   *
   * ```
   * ioSuspend { resume: IoCallback => sender.send("Hello world", resume) }
   * ```
   *
   * Or more succinctly:
   *
   * ```
   * ioSuspend(sender.send("Hello world", _))
   * ```
   */
  def ioSuspend(undertowIo: IoCallback => Unit): Task.Suspend[Unit] = {
    println("Suspending until Undertow calls back")
    Task.suspend { k: Continuation[Unit] =>
      undertowIo(new IoCallback {
        override def onComplete(exchange: HttpServerExchange, sender: Sender): Unit =
          k.resume(())
        override def onException(exchange: HttpServerExchange, sender: Sender, exception: IOException): Unit =
          k.resumeWithException(exception)
      })
    }
  }

  def startWrapped(tracer: Tracer)(f: Request => Task[Response]): Unit = {

    start(tracer) { exchange: HttpServerExchange =>

      val request = new Request {
        override def method: String = exchange.getRequestMethod.toString
        override def uri: String = exchange.getRequestURI
        override def path: String = exchange.getRequestPath
        override lazy val entity: Trickle[Byte] = {
          new StreamSourceChannelTasks(exchange.getRequestChannel()).stream()
        }
      }
      f(request).flatMap { response: Response =>

        Task.flatEval {
          def sendRead(r: Read[Byte]): Task[Unit] = {
            val sender: Sender = exchange.getResponseSender()
            new SenderTasks(sender).send(r)
          }

          val bodyStream: Trickle[Byte] = response.body
          val bodyRead: Read[Byte] = Read.fromStream(bodyStream)
          bodyRead match {
            case Read.Done =>
              println("Undertow backend: response body is Done")
              exchange.setStatusCode(response.statusCode)
              Task.Unit
            case Read.Error(t) =>
              println("Undertow backend: response body is Error")
              exchange.setStatusCode(500)
              t.printStackTrace()
              Task.Failure(t)
            case available: Read.Available[Byte] =>
              println(s"Undertow backend: response body has available data: ${available.piece}")
              sendRead(available)
            case computed: Read.Computed[Byte] =>
              // "Dispatch" to same thread to avoid exchange being ended automatically. Staying on the same thread
              // makes it possible to do some processing without paying the price of a thread context switch. It is the
              // responsibility of the caller to avoid blocking the IO thread by switching to another thread if
              // necessary.
              println(s"Undertow backend: response body needs computing")
              Task.suspend { resume: Continuation[Unit] =>
                exchange.dispatch(SameThreadExecutor.INSTANCE, new Runnable() {
                  override def run(): Unit = resume.resume(())
                })
              }.flatThen { sendRead(computed) }
            case otherRead =>
              val sender: Sender = exchange.getResponseSender()
              new SenderTasks(sender).send(otherRead)
          }
        }.finallyTask(response.onDone)

      }
    }
  }

  def start(tracer: Tracer)(f: HttpServerExchange => Task[Unit]): Unit = {

    def httpHandler = new HttpHandler {

      override def handleRequest(exchange: HttpServerExchange): Unit = {
        val span: ActiveSpan = tracer.buildSpan("handle_request").startActive()
        tracer.extract(
          Format.Builtin.HTTP_HEADERS,
          new TextMap {
            override def put(key: String, value: String): Unit = ???
            override def iterator(): util.Iterator[Map.Entry[String, String]] = {
              new util.Iterator[Map.Entry[String,String]] {
                val headerIterator = exchange.getRequestHeaders.iterator()
                override def hasNext: Boolean = headerIterator.hasNext
                override def next(): Map.Entry[String, String] = {
                  val headerValues = headerIterator.next()
                  new Map.Entry[String, String] {
                    override def getKey: String = headerValues.getHeaderName.toString
                    override def getValue: String = {
                      assert(headerValues.size() == 1, s"Expected only one header of name ${headerValues.getHeaderName}, got ${headerValues.size}")
                      headerValues.get(0)
                    }
                    override def setValue(value: String): String = ???
                  }
                }
              }
            }
          }
        )
        val t: Task[Unit] = Task.Success(exchange).flatMap(f).`catch` {
          case NonFatal(t) =>
            System.err.println("Failure handling request")
            t.printStackTrace()
        }.`finally` {
//          tracer.inject(
//            span.context(),
//            Format.Builtin.HTTP_HEADERS,
//            new TextMap {
//              override def put(key: String, value: String): Unit = {
//                exchange.getResponseHeaders.put(new HttpString(key), value)
//              }
//              override def iterator(): util.Iterator[Map.Entry[String, String]] = ???
//            }
//          )
          exchange.endExchange()
        }
        val listener = new OpenTracingInterpreterListener(tracer)
        new FunctionalInterpreter(listener).run(t)
      }
    }

    val port = 8000
    val server: Undertow = Undertow.builder()
        .addHttpListener(port, "localhost")
        .setHandler(httpHandler)
        .build()
    server.start()
    System.err.println(s"Listening on port $port")
  }
}
