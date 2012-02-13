package com.promindis.user

import com.promindis.patterns.{Functor, Monad, Applicative}
import java.util.concurrent.{TimeUnit, Future, Callable}
import Applicative._

/**
 * Date: 06/02/12
 * Time: 20:02
 */

case class W(value: Int) extends Callable[Int] {
  def call() = {
    Thread.sleep(3000)
    value
  }
}

object Futures {
  implicit object ApplicativeFuture extends Applicative[Future] with Monad[Future] with Functor[Future] {
    def apply[T](data: T) = new Future[T] {
      override def get() = data

      override def get(timeout: Long, unit: TimeUnit) = get()

      override def cancel(mayInterruptIfRunning: Boolean) = false

      override def isCancelled = false

      override def isDone = true
    }

    def flatten[T](m: Future[Future[T]]) = m.get()

    def map[T, P >: T, U](source: Future[T])(f: (P) => U) = new Future[U] {
      override def get() = f(source.get())

      override def get(timeout: Long, unit: TimeUnit) = f(source.get(timeout, unit))

      override def cancel(mayInterruptIfRunning: Boolean) = false

      override def isCancelled = false

      override def isDone = true
    }
  }


}
