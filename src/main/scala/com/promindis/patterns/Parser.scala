package com.promindis.patterns

trait Parser[+A] {
  def apply(input: String): List[(A, String)]
}

object Parser {
  implicit def toParser[A](parser: Parser[A]) = new {
    def map[B](f: A => B) = MonadicParser.map(parser)(f)

    def flatMap[B](f: A => Parser[B]) = MonadicParser.flatMap(parser)(f)(MonadicParser)
  }

  def satisfy(predicate: Char => Boolean) = item.flatMap{ x =>
    if (predicate(x)) Result(x)
    else zero
  }

  def char(c : Char) = satisfy{_ == c}

  def digit = satisfy{_.isDigit}

  def lower = satisfy{_.isLower}

  def upper = satisfy{_.isUpper}
}

case class Result[A](value: A) extends Parser[A] {
  def apply(input: String) = List((value, input))
}

object zero extends Parser[Any] {
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
    def apply(input: String) = {
      val List((parserQ, rest)) = parserP(input)
      parserQ(rest)
    }
  }

  def map[T, P >: T, U](source: Parser[T])(f: (P) => U) = new Parser[U] {
    def apply(input: String) =
      source(input) map { pair => (f(pair._1), pair._2)}
    }
}


