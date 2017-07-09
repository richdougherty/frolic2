package nz.rd.frolic.async

import scala.collection.immutable

final class Context(untyped: immutable.Map[Context.Key[_], _]) {
  import Context.Key
  def apply[T](k: Key[T]): T = untyped(k).asInstanceOf[T]
  def get[T](k: Key[T]): Option[T] = untyped.get(k).asInstanceOf[Option[T]]
  def updated[T](k: Key[T], v: T): Context = new Context(untyped.updated(k, v))

}

object Context {
  val empty: Context = new Context(immutable.Map.empty)
  final class Key[T](name: String)

  private val threadLocal = new ThreadLocal[Context]
  def current: Context = {
    val c = threadLocal.get
    if (c == null) Context.empty else c
  }
  def current_=(newContext: Context): Unit = {
    threadLocal.set(newContext)
  }

  def apply[T](k: Key[T]): T = current(k)
  def update[T](k: Key[T], v: T): Unit = current = current.updated(k, v)
}

