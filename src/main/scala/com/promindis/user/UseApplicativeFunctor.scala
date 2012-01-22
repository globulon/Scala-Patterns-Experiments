package com.promindis.user

import com.promindis.patterns.{ApplicativeHelper,  Applicative}



//
object UseApplicativeFunctor {
  implicit object ApplicativeList extends ApplicativeHelper[List] {

    override def flatten[T](m: List[List[T]]) = m.flatten

    override def toApplicative[T](c: List[T]) = listToApplicative(c)

    implicit def listToApplicative[A](list: List[A]) = new Applicative[A, List] {
      def map[U](f: (A) => U) = list.map(f)
    }



  }

  implicit object ApplicativeOption extends ApplicativeHelper[Option] {
    override def flatten[T](m: Option[Option[T]]) =
      m match {
        case Some(opt) => opt
        case _ => None
      }

    override def toApplicative[T](c: Option[T]) = optionToApplicative(c)

    implicit def optionToApplicative[A](option: Option[A]) = new Applicative[A, Option] {
      def map[U](f: (A) => U) = option.map(f)
    }
  }

  import ApplicativeList._
  import ApplicativeOption._
  import Applicative._

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