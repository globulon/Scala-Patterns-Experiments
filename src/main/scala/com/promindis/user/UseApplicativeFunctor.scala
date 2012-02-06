package com.promindis.user

import com.promindis.patterns._
import java.util.concurrent._


case class W(value: Int) extends Callable[Int] {
  def call() = {
    Thread.sleep(3000)
    value
  }
}

object UseApplicativeFunctor {

  implicit object ApplicativeFuture extends Applicative[Future] with Monad[Future] with Functor[Future] {
    def apply[T](data: T) = new Future[T] {
      override def get() = data

      override def get(timeout: Long, unit: TimeUnit) = get()

      override def cancel(mayInterruptIfRunning: Boolean) = false

      override def isCancelled = false

      override def isDone = true
    }

    def flatten[T](m: Future[Future[T]]) = m.get()

    def map[T, U](source: Future[T])(f: (T) => U) = new Future[U] {
      override def get() = f(source.get())

      override def get(timeout: Long, unit: TimeUnit) = f(source.get(timeout, unit))

      override def cancel(mayInterruptIfRunning: Boolean) = false

      override def isCancelled = false

      override def isDone = true
    }
  }

  def mul = (a: Int) => a * 10
  def add = (a: Int, b: Int) => a + b
  def addd = (a: Int, b: Int, c: Int) => a + b + c

  def exec(workers: W*)(implicit pool: ExecutorService): List[Int] = {
    workers.toList.map {pool.submit(_)}.sequenceA.get()
  }


  def main(args: Array[String]) {
    import Applicative._

    println(liftA2(add, List(1, 2), List(3, 4)))
    println(liftA3(addd, List(1, 2), List(3, 4), List(5, 6)))
    println(add :@: List(1, 2) :*: List(3, 4))
    println(addd :@: List(1, 2) :*: List(3, 4) :*: List(5, 6))

    val list: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(list.sequenceA)

    implicit val pool = Executors.newCachedThreadPool()

    val start = System.currentTimeMillis

    val result = add:@:exec(W(1), W(2), W(3)):*:exec(W(4), W(5), W(6))

    val ellapsed = System.currentTimeMillis - start
    println("Got " + result + " in " + ellapsed + " ms")

    pool.shutdown()
  }
}