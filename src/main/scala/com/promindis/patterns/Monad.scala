package com.promindis.patterns

trait Monad[M[_]] {

  def apply[T](data: T): M[T]

  def flatten[T](m: M[M[T]]): M[T]

  def flatMap [T, U](source: M[T])(t: T => M[U])(implicit f: Functor[M]): M[U] = {
    flatten(f.map(source)(t))
  }
}

object  Monad {

  implicit def monadsOp[M[_]: Functor : Monad, T](source: M[T]) = new {
    val monad = implicitly[Monad[M]]
    def flatMap[U](f: T => M[U]): M[U] = monad.flatMap(source)(f)
  }
}




