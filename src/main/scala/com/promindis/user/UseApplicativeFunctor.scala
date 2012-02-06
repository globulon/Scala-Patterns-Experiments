package com.promindis.user
import com.promindis.patterns._

object UseApplicativeFunctor {

  def add = (a: Int, b: Int) => a + b
  def addd = (a: Int, b: Int, c: Int) => a + b + c

  def main(args: Array[String]) {
   import Applicative._

    println(liftA2(add, List(1, 2), List(3, 4)))
    println(liftA3(addd, List(1, 2), List(3, 4), List(5,6)))
    println({_ + _}:@:List(1,2):*:List(3,4))
    println({_ + _ + _}:@:List(1,2):*:List(3,4):*:List(5,6))


    val list: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(list.sequenceA)
  }
}