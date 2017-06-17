package nz.rd.frolic.async

sealed trait Transform[-A,+B]

object Transform {

  implicit def function[A,B](f: Task.Completion[A] => Task[B]) = Function(f)
  def mapped[A,B](f: A => B) = Map(f)
  def fixed[B](t: Task[B]) = Fixed(t)

  case class Cons[-A,B,+C](left: Transform[A,B], right: Transform[B,C]) extends Transform[A,C]
  case class Function[-A,+B](f: Task.Completion[A] => Task[B]) extends Transform[A,B]
  case class Map[-A,+B](f: A => B) extends Transform[A,B]
  case class Fixed[+B](t: Task[B]) extends Transform[Any,B]

}