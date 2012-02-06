package com.promindis.patterns

trait Functor[M[_]] {
  def map[T, U](source: M[T])(f: T => U): M[U];
}

