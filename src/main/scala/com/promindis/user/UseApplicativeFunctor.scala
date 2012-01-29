package com.promindis.user

import com.promindis.patterns.{ApplicativeHelper, MonadHelper, Applicative}


object UseApplicativeFunctor {

  implicit object ApplicativeList extends ApplicativeHelper[List]{
    def flatten[T](m: List[List[T]]) = m.flatten

    implicit def unit[U](data: U) = List(data)

    implicit def toApplicative[T](list: List[T]): Applicative[T, List] = new Applicative[T, List] {
      implicit val applicativeHelper = ApplicativeList

      def map[U](f: (T) => U) = list map f
    }
  }


    def main(args: Array[String]) {
      import Applicative._
      import ApplicativeList._

      val f: Int => Int = _ * 10
      val g: Int => Int = _ + 100
      val h: Int => Int = _ ^ 2

      println(List(f, g, h) :*: List(1, 2, 3))
//      println(Set(f, g, h) :*: Set(1, 2, 3))

      def add(a: Int)(b: Int): Int = a + b
      def uadd = Function.uncurried(add _)
      def mul(a: Int)(b: Int): Int = a * b

      println((List(add _, mul _) :*: List(1, 2)) :*: List(3, 4))

      println(f :@: List(1, 2))

      println((add _ :@: List(1, 2)):*: List(3,4))

      println(liftA2(uadd, List(1, 2), List(3,4)))
    }
}