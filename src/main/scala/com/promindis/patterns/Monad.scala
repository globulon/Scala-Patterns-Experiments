package com.promindis.patterns


trait MonadHelper[M[_]] {
  def flatten[T](m: M[M[T]]): M[T]
}

trait Monad[T, M[_]] extends Functor[T, M]{
  def flatMap [U](f: T => M[U])(implicit helper: MonadHelper[M]): M[U] = helper.flatten(map(f))
}




