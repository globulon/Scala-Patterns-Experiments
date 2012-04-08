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
  } yield Seq(chr, chr2)

  def main(args: Array[String]) {

    println(pickUpper("Hello"))
    println(picLowers("abcd"))
    println(letter("AbcDe"))
    println(word("Yes!"))
    println(neg("-123"))
    println(int("123"))
    println((enumeration(int, char(',')))("1,2,3"))
    println((bracket(char('['), enumeration(int, char(',')), char(']')))("[1,2,3,4]"))
  }

}
