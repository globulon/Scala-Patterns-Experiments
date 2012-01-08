package com.promindis.patterns

trait Functor[T, M[_]] {
  def map[U](f: T => U): M[U];
}