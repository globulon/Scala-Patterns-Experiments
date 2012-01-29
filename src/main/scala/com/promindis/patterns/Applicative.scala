package com.promindis.patterns

trait Applicative[T, F[_]] extends  Monad[T, F]{

  implicit def monadHelper : MonadHelper[F]

  def :*:[U](fs: F[T => U])(implicit c: F[T => U] => Applicative[T => U, F]): F[U] =  {
    flatMap(x => fs.map(f => f(x)))
  }

  def :@:[R](f: T => R)  = map(f)
}

object Applicative {

  def liftA2[T, P, Q, A[_]](f: (T, P) => Q,  a1: A[T], a2:  A[P])
                           (implicit c0: A[T] => Applicative[T, A], c1: A[P] => Applicative[P, A], c: A[P => Q] => Applicative[P => Q, A]): A[Q] =  {
    (f.curried :@: a1) :*: a2
  }

  def sequence[T, A[_]](input: List[A[T]])(implicit c: List[T] => A[List[T]], c0: A[T] => Applicative[T, A], c1: A[List[T]] => Applicative[List[T], A], c2: A[List[T] => List[T]] => Applicative[List[T] => List[T], A]): A[List[T]] = {
    input match {
      case (x::xs) =>
        def cons(head: T, list: List[T]): List[T] = head::list
        liftA2(cons, x, sequence(xs))
      case _ => List.empty[T]
    }
  }

}



