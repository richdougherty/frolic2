package nz.rd.frolic.integrations.opentracing

import io.opentracing.ActiveSpan
import io.opentracing.mock.{MockSpan, MockTracer}
import nz.rd.frolic.async.integration.scala.concurrent.ScalaConcurrentTasks
import nz.rd.frolic.async.{FunctionalInterpreter, Task}
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.annotation.tailrec
import scala.concurrent.Future

class SuspendableActiveSpanSourceSpec extends FreeSpec with Matchers with ScalaFutures {

  /** Helper for testing tracing */
  private class Tracing {
    val activeSpanSource = new SuspendableActiveSpanSource()
    val tracer = new MockTracer(activeSpanSource)

    def makeActive(name: String): SuspendableActiveSpan = tracer.buildSpan(name).startActive().asInstanceOf[SuspendableActiveSpan]
    def optActiveSpan: Option[SuspendableActiveSpan] = Option(activeSpanSource.activeSpan()).map(_.asInstanceOf[SuspendableActiveSpan])
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

  "SuspendableActiveSpanSource" - {
    "should support makeActive/deactivate" in {
      val tracing = new Tracing
      tracing.spanNames should be(Nil)
      tracing.activeSpanSource.activeSpan should be(null)

      val x = tracing.makeActive("x")
      tracing.spanNames should be("x"::Nil)
      val y = tracing.makeActive("y")
      tracing.spanNames should be("y"::"x"::Nil)
      y.deactivate()
      tracing.spanNames should be("x"::Nil)
      x.deactivate()
      tracing.spanNames should be(Nil)
    }
    "should support ActiveSpan.capture/activate" in {
      val tracing = new Tracing

      tracing.optSpanName should be(None)

      tracing.activeSpanSource.activeSpan should be(null)
      val x = tracing.makeActive("x")
      tracing.optSpanName should be(Some("x"))
      val y = tracing.makeActive("y")
      tracing.optSpanName should be(Some("y"))
      val yk = y.capture()
      y.deactivate()
      tracing.optSpanName should be(Some("x"))
      x.deactivate()

      tracing.optSpanName should be(None)
      tracing.activeSpanSource.activeSpan should be(null)

      val y2 = yk.activate()
      tracing.optSpanName should be(Some("y"))
      y2.deactivate()

      tracing.optSpanName should be(None)
      tracing.activeSpanSource.activeSpan should be(null)
    }

    "should support captureStack/deactivateStack, Stack.activate" in {
      val tracing = new Tracing

      tracing.optSpanName should be(None)

      tracing.activeSpanSource.activeSpan should be(null)
      val x = tracing.makeActive("x")
      tracing.spanNames should be("x"::Nil)
      val y = tracing.makeActive("y")
      tracing.spanNames should be("y"::"x"::Nil)

      // Capture the stack x/y
      val stack = tracing.activeSpanSource.captureStack()

      // Check that x/y are still there
      tracing.spanNames should be("y"::"x"::Nil)
      tracing.activeSpanSource.deactivateStack()
      tracing.spanNames should be(Nil)

      tracing.activeSpanSource.activeSpan should be(null)

      // Put spans back again
      stack.activate()

      // Check that x/y are back again
      tracing.spanNames should be("y"::"x"::Nil)
      tracing.activeSpanSource.deactivateStack()
      tracing.spanNames should be(Nil)
    }
  }
}
