package com.promindis.patterns


trait ApplicativeBuilder[F[_]] extends MonadHelper[F]{
  def apply[T](f: F[T]): Applicative[T, F]
}

trait Applicative[T, F[_]] extends  Monad[T, F]{

  def :*:[U](fs: F[T => U])(implicit b: ApplicativeBuilder[F]): F[U] =
    flatMap(x => b(fs).map(f => f(x)))


  def :@:[R](f: T => R)  = map(f)
}

object Applicative {
  def liftA2[T, P, Q, F[_]](f: (T, P) => Q, a1: F[T], a2:  F[P])(implicit b: ApplicativeBuilder[F]) = {
    (f.curried :@: b(a1)) :*: b(a2)
  }


}




