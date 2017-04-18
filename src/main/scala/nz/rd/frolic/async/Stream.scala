package nz.rd.frolic.async

sealed trait Reader[-A,+B]
object Reader {
  case class ReadElement[-A,+B](f: A --> Reader[A,B]) extends Reader[A,B]
  case class ReadChunk[-A,+B](max: Int, f: Seq[A] --> Reader[A,B]) extends Reader[A,B]
  case class Close[+B](f: Unit --> B) extends Reader[Nothing,B]
  case class Error[+B](cause: Throwable, f: Unit --> B) extends Reader[Nothing,B]
}



sealed trait Writer[+B]
object Writer {
  case class WriteElement[+B](element: B, f: _ --> Writer[B]) extends Writer[B]
  case class WriteChunk[+B](chunk: Seq[B], f: Int --> Writer[B]) extends Writer[B]
  case class Close[+B](f: _ --> B) extends Writer[B]
  case class Error[+B](cause: Throwable, f: _ --> B) extends Writer[B]
}

sealed trait Piper[-A,+B]
object Piper {
  case class ReadElement[-A,+B](f: A --> Piper[A,B]) extends Piper[A,B]
  case class ReadChunk[-A,+B](max: Int, f: Seq[A] --> Piper[A,B]) extends Piper[A,B]
  case class WriteElement[-A,+B](element: B, f: Unit --> Piper[A,B]) extends Piper[A,B]
  case class WriteChunk[-A,+B](chunk: Seq[B], f: Int --> Piper[A,B]) extends Piper[A,B]
  case class Close[-A,+B](f: Unit --> B) extends Piper[A,B]
  case class Error[-A,+B](cause: Throwable, f: Unit --> B) extends Piper[A,B]
}