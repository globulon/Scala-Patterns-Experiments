package com.promindis.patterns


trait Monoid[T] {
  def add(x: T, y: T): T
  def unit: T
  def concat(xs: Traversable[T]): T =
    if (xs.isEmpty) unit else xs.fold(unit){add(_, _)}
}

case class Accumulator[A, T](value: T, f: (A) => T)(implicit m: Monoid[T]){
  def this(f: (A) => T)(implicit m: Monoid[T]) = this(m.unit, f)

  def apply(a: A) = copy(value = m.add(f(a), value))
}

