package com.promindis.user

import com.promindis.patterns.{ApplicativeBuilder, Applicative}


object UseApplicativeFunctor {
  implicit object ApplicativeListBuilder extends ApplicativeBuilder[List]{

    implicit def apply[T](list : List[T]) = new Applicative[T, List] {
      override def map[U](f: (T) => U) = list.map(f)
    }

    def flatten[T](m: List[List[T]]) = m.flatten
  }



  import Applicative._
  import ApplicativeListBuilder._

  def main(args: Array[String]) {
    val f: Int => Int = _ * 10
    val g: Int => Int = _ + 100
    val h: Int => Int = _ ^ 2

    println(List(f, g, h) :*: List(1, 2, 3))

    def add(a: Int)(b: Int): Int = a + b
    def uadd = Function.uncurried(add _)
    def mul(a: Int)(b: Int): Int = a * b
    //    val j: Int => Int => Int = _ * _
    println((List(add _, mul _) :*: List(1, 2)) :*: List(3, 4))

    println(f :@: List(1, 2))

    println((add _ :@: List(1, 2)):*: List(3,4))
    println(liftA2(uadd, List(1, 2), List(3,4)))
  }
}