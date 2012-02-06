package com.promindis.user

import com.promindis.patterns._
import java.util.concurrent._


case class Worker(value: Int) extends Callable[Int] {
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

  implicit def adhocPool(pool: ExecutorService) = new {
    def exec(workers: Worker*): Future[scala.List[Int]] = {
      workers.toList.map {pool.submit(_)}.sequenceA
    }
  }


  def main(args: Array[String]) {
    import Applicative._

    println(liftA2(add, List(1, 2), List(3, 4)))
    println(liftA3(addd, List(1, 2), List(3, 4), List(5, 6)))
    println(add :@: List(1, 2) :*: List(3, 4))
    println(addd :@: List(1, 2) :*: List(3, 4) :*: List(5, 6))

    val list: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(list.sequenceA)

    val pool = Executors.newCachedThreadPool()

    val start = System.currentTimeMillis
    val future1 = pool.exec(Worker(1), Worker(2), Worker(3))
    val future2 = pool.exec(Worker(4), Worker(5), Worker(6))

    val result = add:@:future1.get():*:future2.get()

    val ellapsed = System.currentTimeMillis - start
    println("Got " + result + " in " + ellapsed + " ms")

    pool.shutdown()
  }
}