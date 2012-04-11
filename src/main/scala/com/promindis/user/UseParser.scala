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
    println(ints("[1,2,3,4,5]"))
    println(ints2("[1,2,3,4,5]"))
    println((bracket(char('('),for {f <- addop ; x <- int; y <- int} yield (f(x, y)), char(')')))("(+12)"))


//    println(expr("2+5-4+7"))

//    println((for {
//      x ← factor
//      fs ← many((for { f ← addop; y ← factor} yield (f, y)) )
//    } yield (fs.foldLeft(x) { (acc, pair) =>
//      val (f, y ) = pair
//      f(acc, y)
//    }))("1+2+3"))

    println(expr("1+2+3-4"))
    println(expr("1+2+(3-4)"))
    println(first(expr)("1+2+(3-4)"))

    println(parse(symbol("where"))("    where  "))
  }

}
