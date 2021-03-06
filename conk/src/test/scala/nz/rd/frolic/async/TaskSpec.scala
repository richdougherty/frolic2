package nz.rd.frolic.async

import nz.rd.frolic.async.integration.scala.concurrent.ScalaConcurrentTasks
import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

class TaskSpec extends FreeSpec with Matchers with ScalaFutures {

  "Task" - {
    def runToFuture[A](t: Task[A]): Future[A] = {
      ScalaConcurrentTasks.interpretFuture(t, new FunctionalInterpreter(InterpreterListener.nop))
    }
    "Return should yield value" in {
      val t = Task.Success(1)
      runToFuture(t).eitherValue should be (Some(Right(1)))
    }
    "Throw should throw exception" in {
      val e = new Exception
      val t: Task[Unit] = Task.Failure(e)
      runToFuture(t).eitherValue should be (Some(Left(e)))
    }
    "Do should run thunk" in {
      val t = Task.flatEval(Task.Success(9))
      runToFuture(t).eitherValue should be (Some(Right(9)))
    }
    "Task.map should modify value" in {
      val t = Task.Success(1).map(_ + 1)
      runToFuture(t).eitherValue should be (Some(Right(2)))
    }
    "Task.flatMap should modify value" in {
      val t = Task.Success(1).flatMap(x => Task.Success(x + 3))
      runToFuture(t).eitherValue should be (Some(Right(4)))
    }
    "Task.andThen should run success in succession" in {
      val buffer = ArrayBuffer[Int]()
      val t = Task.eval(buffer += 1).`then` {
        buffer += 2
        3
      }
      runToFuture(t).eitherValue should be (Some(Right(3)))
      buffer.toList should be (List(1, 2))
    }
  }

}
