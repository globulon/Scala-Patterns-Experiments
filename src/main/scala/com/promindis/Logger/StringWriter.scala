package com.promindis.Logger

import com.promindis.patterns.{MonadHelper, Monoid, Monad}


class StringWriter[T](val value: T, val log: String)  extends Writer[T, String] with Monad[T, StringWriter]{
  def this(value: T)(implicit monoid: Monoid[String]) = this(value, monoid.unit)

  override def map[U](f: (T) => U) =  StringWriter(f(context._1), context._2)

  override def toString = "[" + value + ", " + log + "]"
}

object StringWriter {
  def apply[T](value: T) = new StringWriter[T](value)

  def apply[T](value: T, log: String) = new StringWriter[T](value, log)

  implicit object writerMonadHelper extends MonadHelper[StringWriter] {
    def flatten[T](m: StringWriter[StringWriter[T]]) = {
      val pair = m.context
      val innerW = pair._1
      val innerPair = innerW.context
      StringWriter(innerPair._1, m.combined(innerPair._2))
    }
  }

}