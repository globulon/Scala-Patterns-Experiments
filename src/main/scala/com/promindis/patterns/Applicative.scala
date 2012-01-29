package com.promindis.patterns

trait ApplicativeBuilder[F[_]] {
  implicit def apply[U](fs: F[U]): Applicative[U, F]

  implicit def wrap[U](data: U): F[U]
}

trait Applicative[T, F[_]] extends  Monad[T, F]{

  implicit def monadHelper : MonadHelper[F]


  def :*:[U](fs: F[T => U])(implicit b: ApplicativeBuilder[F]): F[U] =  {
    flatMap(x => b(fs).map(f => f(x)))
  }

  def :@:[R](f: T => R)  = map(f)
}

object Applicative {

  def liftA2[T, P, Q, A[_]](f: (T, P) => Q,  a1: A[T], a2:  A[P])(implicit b: ApplicativeBuilder[A]): A[Q] =  {
    (f.curried :@: b(a1)) :*: b(a2)
  }

  def sequence[T, A[_]](input: List[A[T]])(implicit b: ApplicativeBuilder[A]): A[List[T]] = {
    input match {
      case (x::xs) =>
        def cons(head: T, list: List[T]): List[T] = head::list
        liftA2(cons, x, sequence(xs))
      case _ => b.wrap(List.empty[T])
    }
  }

}




