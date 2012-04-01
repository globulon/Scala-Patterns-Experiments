package com.promindis.user
import com.promindis.patterns._
import Parser._
/**
 * Date: 01/04/12
 * Time: 14:49
 */

object  UseParser {

  def pickUpper = for {
    chr ← upper
  } yield chr

  def picLowers = for {
    chr ← lower
    chr2 ← lower
  } yield (println(chr + chr2))


  def main(args: Array[String]) {

    println(pickUpper("Hello"))
    println(picLowers("abcd"))

  }

}
