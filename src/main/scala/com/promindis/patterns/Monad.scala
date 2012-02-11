package com.promindis.patterns

trait Monad[M[_]] {

  def apply[T](data: T): M[T]

  def flatten[T](m: M[M[T]]): M[T]

  final def flatMap [T, U](source: M[T])(t: T => M[U])(implicit f: Functor[M]): M[U] = {
    flatten(f.map(source)(t))
  }
}





