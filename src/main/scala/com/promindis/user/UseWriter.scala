package com.promindis.user

import com.promindis.Logger.Writer._
import com.promindis.patterns._

object UseWriter {

  def logNumber(x: Int): StringWriter[Int] = StringWriter[Int](x, "Got number " + x + " ")

  def main(arguments: Array[String]) {

    val value = for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b

    println(value)
    println(
      logNumber(3).flatMap(x =>
        logNumber(5).map(y =>
          x * y)))

    val anotherValue = for {
      a <- logNumber(3)
      b <- logNumber(5)
      c <- logNumber(7)
    } yield a * b * c

    println(anotherValue)
    println(
      logNumber(3).flatMap(x =>
        logNumber(5).flatMap(y =>
          logNumber(7).map(z =>
            x * y * z))))
  }
}