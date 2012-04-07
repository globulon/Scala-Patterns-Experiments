package com.promindis.patterns


trait Parser[+A] {
  def apply(input: String): List[(A, String)]
}

case class Result[A](value: A) extends Parser[A] {
  def apply(input: String) = List((value, input))
}

object zero extends Parser[Nothing] {
  def apply(input: String) = Nil
}

object item extends Parser[Char] {
  def apply(input: String) = {
    if (input.isEmpty) Nil
    else List((input.head, input.tail))
  }
}

object MonadicParser extends Applicative[Parser] with MonoidC[Parser]{
  def apply[T](data: T) = Result(data)

  def flatten[T](parserP: Parser[Parser[T]]) = new Parser[T] {
    def apply(input: String) = parserP(input) match {
      case List((parserQ, rest)) =>  parserQ(rest)
      case _ => Nil
    }
  }

  def map[T, P >: T, U](source: Parser[T])(f: (P) => U) = new Parser[U] {
    def apply(input: String) =
      source(input) map { pair => (f(pair._1), pair._2)}
  }

  def add[T](k: Parser[T], l: Parser[T]) = new Parser[T] {
    def apply(input: String) = k(input) ++ l(input)
  }

  def unit[T] = zero
}


object Parser {

  def satisfy(predicate: Char => Boolean): Parser[Char] = item.flatMap{ x =>
    if (predicate(x)) Result(x)
    else zero
  }

  def char(c : Char): Parser[Char] = satisfy{_ == c}

  def digit = satisfy{_.isDigit}

  def lower = satisfy{_.isLower}

  def upper = satisfy{_.isUpper}

  def plus[A](p: Parser[A], q: Parser[A]) = new Parser[A] {
    def apply(input: String) = p(input) ++ q(input)
  }

  def letter = plus(lower, upper)

  def alphanum = plus(letter, digit)

  def word: Parser[String] = plus(nonEmptyWord, Result(""))

  def nonEmptyWord: Parser[String] = for {
    l ← letter
    w ← word
  } yield  (l + w)

  def string: Parser[String] = (for {
    l ← letter
    s ← letter
  } yield (Seq[Char](l,s).toString()))


  def str: Parser[String] = MonadicParser.flatMap(letter) { l =>
    MonadicParser.map(str) { s => l + s}}(MonadicParser)

  def many[A](p: Parser[A]): Parser[List[A]] = (for {
    h ← p
    r ← many(p)
  } yield (h::r))++Result(List.empty[A])

  def many1[A](p: Parser[A]): Parser[List[A]] = for {
    h ← p
    r ← many(p)
  } yield (h::r)

  def toInt(chars: Seq[Char]) =  chars.foldRight((1, 0)){(item, acc) =>
    (acc._1 * 10, (item - '0') * acc._1 + acc._2)
  }._2

  def nat: Parser[Int] = for {
    result ← many1(digit)
  } yield (toInt(result))

  def neg: Parser[Int] = for {
    _ ← char('-')
    result ← many1(digit)
  } yield (-toInt(result))

  def int = nat ++ neg

  def ints: Parser[List[Int]] = for {
    _ ← char('[')
    num ← int
    nums ← many (for {_ ← char(',');x ← int} yield(x))
    _ ← char(']')
  } yield (num::nums)

  def enumeration[A, B](matcher: Parser[A], separator: Parser[B]): Parser[List[A]] = for {
    first ← matcher
    rest ← many(for {_ ← separator; x ← matcher} yield x)
  } yield (first::rest)

  def bracket[A, B, C](open: Parser[A], items: Parser[B], close: Parser[C]): Parser[B] = for {
    _ ← open
    result ← items
    _ ← close
  } yield result

  def ints2 = bracket(char('['), enumeration(int, char(',')), char(']'))
}



