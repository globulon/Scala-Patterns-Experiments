package com.promindis.user

import com.promindis.patterns.{ApplicativeBuilder, Applicative}


object UseApplicativeFunctor {
  implicit object ApplicativeListBuilder extends ApplicativeBuilder[Iterable]{

    implicit def apply[T](iterable : Iterable[T]) = new Applicative[T, Iterable] {
      override def map[U](f: (T) => U) = iterable.map(f)
    }

    def flatten[T](m: Iterable[Iterable[T]]) = m.flatten
  }



  import ApplicativeListBuilder._

  def main(args: Array[String]) {

    val f: Int => Int = _ * 10
    val g: Int => Int = _ + 100
    val h: Int => Int = _ ^ 2

    println(List(f, g, h) :*: List(1, 2, 3))

    def add(a: Int)(b: Int): Int = a + b
//    def uadd = Function.uncurried(add _)
    def mul(a: Int)(b: Int): Int = a * b

    println((List(add _, mul _) :*: List(1, 2)) :*: List(3, 4))

    println(f :@: List(1, 2))

    println((add _ :@: List(1, 2)):*: List(3,4))

    println((add _ :@: Set(5, 6)):*: Set(7, 8))
//    println(liftA2(uadd, List(1, 2), List(3,4)))
  }
}