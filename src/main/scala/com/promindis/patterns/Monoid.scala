package com.promindis.patterns


trait Monoid[T] {
  def add(x: T, y: T): T
  def unit: T
  def concat(xs: Traversable[T]): T =
    if (xs.isEmpty) unit else xs.foldLeft(unit){add(_, _)}
}

object Monoid {
  implicit object StringMonoid extends Monoid[String] {
    override def add(x: String, y: String) = x + y
    override def unit = ""
  }

  implicit object IntegerMonoid extends Monoid[Int] {
    override def add(x: Int, y: Int) = x + y
    override def unit = 0
  }

  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    override def add(x: List[T], y: List[T]) = x ++ y
    override def unit = Nil
  }

  def concatenate[T](xs: Traversable[T])(implicit monoid: Monoid[T]) = monoid.concat(xs)
}