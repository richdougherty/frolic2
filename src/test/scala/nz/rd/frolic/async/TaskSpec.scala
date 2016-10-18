package nz.rd.frolic.async

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class TaskSpec extends FreeSpec with Matchers with ScalaFutures {

  "Task" - {
    def runToFuture[A](t: Task[A]): Future[A] = {
      val (newTask, future) = Task.toFuture(t)
      new FunctionalInterpreter().run(newTask)
      future
    }
    "Return should yield value" in {
      val t = Task.Value(1)
      runToFuture(t).eitherValue should be (Some(Right(1)))
    }
    "Throw should throw exception" in {
      val e = new Exception
      val t: Task[Unit] = Task.Throw(e)
      runToFuture(t).eitherValue should be (Some(Left(e)))
    }
    "Do should run thunk" in {
      val t = Task.FlatEval(Task.Value(9))
      runToFuture(t).eitherValue should be (Some(Right(9)))
    }
    "Task.map should modify value" in {
      val t = Task.Value(1).map(_ + 1)
      runToFuture(t).eitherValue should be (Some(Right(2)))
    }
    "Task.flatMap should modify value" in {
      val t = Task.Value(1).flatMap(x => Task.Value(x + 3))
      runToFuture(t).eitherValue should be (Some(Right(4)))
    }
  }

}
