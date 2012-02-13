package com.promindis.patterns

trait Functor[M[_]] {
  def map[T,P >: T, U](source: M[T])(f: P => U): M[U];
}

