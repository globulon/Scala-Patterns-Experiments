package com.promindis.Logger

import com.promindis.patterns.{MonadHelper, Monoid, Monad}


trait Writer[T, M] {
  val value: T
  val log: M
  def context: (T, M) = (value, log)
  def combined(otherLog: M )(implicit monoid: Monoid[M]) = monoid.add(log, otherLog)
}

class StringWriter[T](val value: T, val log: String)  extends Writer[T, String]{
  def this(value: T)(implicit monoid: Monoid[String]) = this(value, monoid.unit)

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

  implicit def writerToMonad[T](w: StringWriter[T]) = new Monad[T, StringWriter] {
    override def map[U](f: (T) => U) = {
      val pair = w.context
      StringWriter(f(pair._1), pair._2)
    }
  }
}

