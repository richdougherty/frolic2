package nz.rd.frolic.backend.undertow

import java.io.IOException

import io.undertow.Undertow
import io.undertow.io.{IoCallback, Sender}
import io.undertow.server.{HttpHandler, HttpServerExchange}
import io.undertow.util.SameThreadExecutor
import nz.rd.frolic.async.trickle.{Read, Trickle}
import nz.rd.frolic.async.{FunctionalInterpreter, Task, trickle, _}
import nz.rd.frolic.http.{Request, Response}

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

  def startWrapped(f: Request => Task[Response]): Unit = {

    start { exchange: HttpServerExchange =>
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

  def start(f: HttpServerExchange => Task[Unit]): Unit = {

    def httpHandler = new HttpHandler {

      override def handleRequest(exchange: HttpServerExchange): Unit = {
        val t: Task[Unit] = Task.Success(exchange).flatMap(f).`catch` {
          case NonFatal(t) =>
            System.err.println("Failure handling request")
            t.printStackTrace()
        }.`finally` {
          exchange.endExchange()
        }
        new FunctionalInterpreter().run(t)
      }
    }

    val server: Undertow = Undertow.builder()
        .addHttpListener(8000, "localhost")
        .setHandler(httpHandler)
        .build()
    server.start()
  }
}
