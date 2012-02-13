package com.promindis.Logger

import com.promindis.patterns.{Functor, Monad, Monoid}


trait Writer[T, M] {
  val value: T
  val log: M
  def context: (T, M) = (value, log)
  def combined(otherLog: M )(implicit monoid: Monoid[M]) = monoid.add(log, otherLog)
}

object Writer {

  implicit object StringWriterMonad extends Monad[StringWriter] with Functor[StringWriter] {

    override def apply[T](value: T) = new StringWriter[T](value)

    override def map[T, P >: T, U](source: StringWriter[T])(f: (P) => U) =
      StringWriter(f(source.context._1), source.context._2)

    override def flatten[T](m: StringWriter[StringWriter[T]]) = {
      val pair = m.context
      val innerW = pair._1
      val innerPair = innerW.context
      StringWriter(innerPair._1, m.combined(innerPair._2))
    }

  }

  case class StringWriter[T](value: T, log: String) extends Writer[T, String]  {

    def this(value: T)(implicit monoid: Monoid[String]) = this(value, monoid.unit)

    override def toString = "[" + value + ", " + log + "]"

  }

}


