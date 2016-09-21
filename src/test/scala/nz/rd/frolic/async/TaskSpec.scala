package nz.rd.frolic.async

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

class TaskSpec extends FreeSpec with Matchers with ScalaFutures {

  "Task" - {
    "Return should yield value" in {
      val t = Task.Return(1)
      Task.runToFuture(t).eitherValue should be (Some(Right(1)))
    }
    "Throw should throw exception" in {
      val e = new Exception
      val t: Task[Unit] = Task.Throw(e)
      Task.runToFuture(t).eitherValue should be (Some(Left(e)))
    }
    "Do should run thunk" in {
      val t = Task.Do(() => Task.Return(9))
      Task.runToFuture(t).eitherValue should be (Some(Right(9)))
    }
    "Task.map should modify value" in {
      val t = Task.Return(1).map(_ + 1)
      Task.runToFuture(t).eitherValue should be (Some(Right(2)))
    }
    "Task.flatMap should modify value" in {
      val t = Task.Return(1).flatMap(x => Task.Return(x + 3))
      Task.runToFuture(t).eitherValue should be (Some(Right(4)))
    }
  }

}
