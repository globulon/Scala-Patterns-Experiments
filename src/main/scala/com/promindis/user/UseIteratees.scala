package com.promindis.user

/**
 * Date: 13/03/12
 * Time: 21:10
 */

import com.promindis.patterns.{IterV, Iteratee}
import Iteratee._

object UseIteratees {

//  def drop1Keep1[E]: IterV[E, Option[E]] = for {
//    h <- head
//  } yield h

  def main(args: Array[String]) {
    println(run(enum(length, List(1,2,3))))
    println(run(enum(first[Int], List[Int](1,2,3))))
    println(enum(drop[Int](1), List[Int](1,2,3)))
  }

}
