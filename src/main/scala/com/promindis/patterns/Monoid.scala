package com.promindis.patterns

trait SemiGroup[T] {
  def add(x: T, y: T): T
}

trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}

trait MonoidC[L[X]] {
  def add[T](k: L[T], l: L[T]): L[T]

  def apply[T](value: T): L[T]

  def unit[T]: L[T]
}








