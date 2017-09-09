package nz.rd.frolic.integrations.opentracing

import io.opentracing.ActiveSpan
import io.opentracing.ActiveSpan.Continuation
import io.opentracing.mock.{MockSpan, MockTracer}
import nz.rd.frolic.async.integration.scala.concurrent.ScalaConcurrentTasks
import nz.rd.frolic.async.{FunctionalInterpreter, Task}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class OpenTracingInterpreterSpec extends FreeSpec with Matchers with ScalaFutures {

  /** Helper for testing tracing */
  private class Tracing {
    val activeSpanSource = new SuspendableActiveSpanSource()
    val mockTracer = new MockTracer(activeSpanSource)
    val tracer = new SuspendableTracer(mockTracer, activeSpanSource)
    val listener = new OpenTracingInterpreterListener(tracer)

    def buildActive(names: String*): Unit = {
      names.foreach { name =>
        tracer.buildSpan(name).startActive().asInstanceOf[SuspendableActiveSpan]
      }
    }

    def optActiveSpan: Option[SuspendableActiveSpan] = Option(tracer.activeSpan()).map(_.asInstanceOf[SuspendableActiveSpan])
    def optSpan: Option[MockSpan] = optActiveSpan.map(_.wrapped.asInstanceOf[MockSpan])
    def optSpanName: Option[String] = optSpan.map(_.operationName)
    def span: MockSpan = optSpan.get
    def spanNames: List[String] = {
      val topSpan: ActiveSpan = activeSpanSource.activeSpan
      topSpan match {
        case null => Nil
        case s =>
          val k = s.capture()
          s.deactivate()
          val name = s.asInstanceOf[SuspendableActiveSpan].wrapped.asInstanceOf[MockSpan].operationName
          val otherSpanNames = spanNames // Capture other span names while top span is removed
          k.activate()
          name :: otherSpanNames
      }
    }
  }

  "OpenTracingInterpreterListener" - {
    "calling callbacks directly" - {
      "Should set task span and restore an empty stack" in {
        val tracing = new Tracing
        tracing.optSpanName should be(None)
        val origLocalStack = tracing.listener.starting()
        tracing.optSpanName should be(Some("task"))
        tracing.listener.completing(origLocalStack)
        tracing.optSpanName should be(None)
      }
      "Should set task span and restore a populated stack" in {
        val tracing = new Tracing
        val x = tracing.buildActive("x")
        val y = tracing.buildActive("y")
        tracing.spanNames should be("y"::"x"::Nil)
        val origLocalStack = tracing.listener.starting()
        tracing.spanNames should be("task"::Nil)
        tracing.listener.completing(origLocalStack)
        tracing.spanNames should be("y"::"x"::Nil)
      }
    }
    "when running in an interpreter" - {

      def runTask[A](t: Task[A], tracing: Tracing): Future[A] = {
        val interpreter = new FunctionalInterpreter(tracing.listener)
        ScalaConcurrentTasks.interpretFuture(t, interpreter)
      }

      "Should set task span and restore empty stack" in {
        val tracing = new Tracing
        tracing.spanNames should be(List())
        runTask(Task.eval {
          tracing.spanNames should be(List("task"))
        }, tracing).eitherValue
        tracing.spanNames should be(List())
      }
      "Should set task span and restore populated stack" in {
        val tracing = new Tracing
        tracing.spanNames should be(List())
        tracing.buildActive("as1", "as2")
        tracing.spanNames should be(List("as2", "as1"))
        runTask(Task.eval {
          tracing.spanNames should be(List("task"))
        }, tracing).eitherValue
        tracing.spanNames should be(List("as2", "as1"))
        tracing.activeSpanSource.deactivateStack()
        tracing.spanNames should be(List())
      }
      "Should work with suspend/resume" in {
        val tracing = new Tracing
        tracing.spanNames should be(List())
        tracing.buildActive("as1", "as2")
        tracing.spanNames should be(List("as2", "as1"))
        runTask(
          for {
            _ <- Task.eval { tracing.spanNames should be(List("task")) }
            _ <- Task.suspend { k: nz.rd.frolic.async.Continuation[Unit] =>
                tracing.spanNames should be(List("suspending"))
                new Thread(() => k.resumeWithThunk {
                  tracing.spanNames should be(List("resuming"))
                  Task.Unit
                }).start()
              }
            _ <- Task.eval { tracing.spanNames should be(List("task")) }
          } yield (), tracing).futureValue
        tracing.spanNames should be(List("as2", "as1"))
        tracing.activeSpanSource.deactivateStack()
        tracing.spanNames should be(List())
      }
    }
  }

}
