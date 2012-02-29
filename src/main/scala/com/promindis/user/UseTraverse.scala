package com.promindis.user

import com.promindis.patterns._
/**
 * Date: 22/02/12
 * Time: 22:42
 */

object ListTraverse extends TraverseListLike[List] with ListLike[List]{

  def empty[T]() = Nil

  def cons[T](x: T, xs: List[T]) = x::xs

  def first[T](source: List[T]) = source.headOption

  def rest[T](source: List[T]) = source.tail
}

object UseTraverse {
  def main(args: Array[String]) {
    val options: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(ListTraverse.sequence(options))
  }
}
