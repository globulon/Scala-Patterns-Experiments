package com.promindis.user
import com.promindis.patterns._
/**
 * Date: 21/02/12
 * Time: 22:27
 */

object ReaderMonad {

  def `*2` = (x: Int) => x * 2
  def `+10` = (x: Int) => 10 + x

  def main(args: Array[String]) {
    println((for {
      a <- `*2`
      b <-  `+10`
    } yield (a + b))(3))
  }
}
