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
}