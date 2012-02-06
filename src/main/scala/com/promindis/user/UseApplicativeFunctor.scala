package com.promindis.user
import com.promindis.patterns._

object UseApplicativeFunctor {

  def add = (a: Int, b: Int) => a + b
  def addd = (a: Int, b: Int, c: Int) => a + b + c

  def main(args: Array[String]) {
   import Applicative._

    println((add.curried:@:List(1,2)):*:List(3,4))
    println(((addd.curried:@:List(1,2)):*:List(3,4)):*:List(5,6))

    println(lift(add, List(1, 2), List(3, 4)))
    val list: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(sequence(list))
  }
}