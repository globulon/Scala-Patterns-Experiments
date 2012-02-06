package com.promindis.user

import com.promindis.patterns._
import java.util.concurrent._
import Applicative._
import Futures._


object UseApplicativeFunctor {


  def mul = (a: Int) => a * 10
  def add = (a: Int, b: Int) => a + b
  def addd = (a: Int, b: Int, c: Int) => a + b + c

  def exec(workers: W*)(implicit pool: ExecutorService): List[Int] = {
    workers.toList.map {pool.submit(_)}.sequenceA.get()
  }


  def basics() {
    println(liftA2(add, List(1, 2), List(3, 4)))
    println(liftA3(addd, List(1, 2), List(3, 4), List(5, 6)))
    println(add :@: List(1, 2) :*: List(3, 4))
    println(addd :@: List(1, 2) :*: List(3, 4) :*: List(5, 6))
  }

  def futures() {
    implicit val pool = Executors.newCachedThreadPool()

    val start = System.currentTimeMillis

    val result = add :@: exec(W(1), W(2), W(3)) :*: exec(W(4), W(5), W(6))

    val ellapsed = System.currentTimeMillis - start
    println("Got " + result + " in " + ellapsed + " ms")

    pool.shutdown()
  }

  def getConnection(input: List[Option[String]]) = {
      def doSomething(fromParameters: List[String]) =  fromParameters.toString()

      for {
        withParams <- input.sequenceA
      } yield doSomething(withParams)
  }

  def main(args: Array[String]) {

    basics()

    val list: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(list.sequenceA)

    println(getConnection(List(Some("url"), Some("user"), Some("passwd"))))
    println(getConnection(List(Some("url"), None, Some("passwd"))))


    futures()
  }
}