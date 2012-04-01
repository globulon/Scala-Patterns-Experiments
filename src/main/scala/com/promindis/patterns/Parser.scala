package com.promindis.patterns

trait Parser[+A] {
  def apply(input: String): List[(A, String)]
}

object Parser {

  def satisfy(predicate: Char => Boolean): Parser[Char] = item.flatMap{ x =>
    if (predicate(x)) Result(x)
    else zero
  }

  def char(c : Char) = satisfy{_ == c}

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

}

case class Result[A](value: A) extends Parser[A] {
  def apply(input: String) = List((value, input))
}

object zero extends Parser[Nothing] {
  def apply(input: String) = List.empty
}

object item extends Parser[Char] {
  def apply(input: String) =
    if (input.isEmpty) List.empty
    else List((input.head, input.tail))
}

object MonadicParser extends Applicative[Parser] {
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
}


