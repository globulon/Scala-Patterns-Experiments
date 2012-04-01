package com.promindis.user

/**
 * Date: 13/03/12
 * Time: 21:10
 */

import com.promindis.patterns._
import Iteratee._

object UseIteratees {

  def drop1Keep1[E]: IterV[E, Option[E]] =  for {
    _: Unit <- drop[E](1)
    h: Option[E] <- head[E]
  } yield h


  def repeat[E, A](iter: IterV[E, A], n: Int): IterV[E, List[A]] = {
    type P[X] = IterV[E, X]
    implicit val appl = iterateesToApplicative[E]()
    replicateM[A, P, List](iter, n)
  }


  def main(args: Array[String]) {
    println(run(enum(length, List(1,2,3))))
    println(run(enum(head[Int], List(1,2,3))))
    println(enum(drop[Int](1), List(1,2,3)))
    println(run(enum(drop1Keep1[Int], List(1,2,3))).flatten)
    println(run(enum(drop1Keep1[Int], List(1,2,3,4,5))))

    println(run(enum(repeat(drop1Keep1[Int], 5), List(1,2,3,4,5,6,7,8,9,10))).flatten.flatten)

  }

}
