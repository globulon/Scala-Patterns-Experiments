package com.promindis.user

import com.promindis.patterns.{ApplicativeHelper, Applicative}

object Applicatives {
  implicit object ApplicativeList extends ApplicativeHelper[List]{
    override def flatten[T](m: List[List[T]]) = m.flatten

    override def apply[U](data: U) = List(data)

    override def apply[T](list: List[T]): Applicative[T, List] = new Applicative[T, List] {
      def map[U](f: (T) => U) = list map f
    }
  }

  implicit object ApplicativeOption extends ApplicativeHelper[Option]{
    override def flatten[T](m: Option[Option[T]]) = m match {
      case Some(Some (data)) => Some(data)
      case _ => None
    }

    override def apply[T](list: Option[T]): Applicative[T, Option] = new Applicative[T, Option] {
      def map[U](f: (T) => U) = list map f
    }

    def apply[U](data: U) = Some(data)
  }
}

object UseApplicativeFunctor {
  import Applicatives._
  implicit def listToApplicative[T](list: List[T]) = ApplicativeList(list)
  implicit def optionToApplicative[T](otions: Option[T]) = ApplicativeOption(otions)

    def main(args: Array[String]) {
      import Applicative._
 
      val f: Int => Int = _ * 10
      val g: Int => Int = _ + 100
      val h: Int => Int = _ ^ 2

      println(List(f, g, h) :*: List(1, 2, 3))

      def add = (a: Int) => (b: Int) => a + b
      def mul = (a: Int) => (b: Int) => a * b

      println((List(add , mul ) :*: List(1, 2)) :*: List(3, 4))

      println((add :@: List(1, 2)):*: List(3,4))
      println((add :@: Some(1)) :*: Some(2))

      def addd = (a: Int) => (b: Int) => (c: Int) => a + b +c
      println(((List(addd  :@: List(1, 2)) :*: List(3, 4)) :*: List(5,6)))

      def uadd = (a: Int, b: Int) => a + b
      println(liftA2(uadd, List(1, 2), List(3,4)))

//      println(sequence(List[Option](Some(1), Some(2), Some(3))))
   }
}