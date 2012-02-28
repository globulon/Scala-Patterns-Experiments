package com.promindis.user

import com.promindis.patterns._
/**
 * Date: 22/02/12
 * Time: 22:42
 */

object UseTraverse {
  def main(args: Array[String]) {
    val options: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    println(Traverser.sequence(options))
  }
}
