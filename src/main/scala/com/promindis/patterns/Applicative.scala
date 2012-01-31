package com.promindis.patterns


trait ApplicativeHelper[F[_]] extends MonadHelper[F] {
  def apply[U](data: U): F[U]
  def apply[U](input: F[U]): Applicative[U, F]
}

trait Applicative[+T, F[_]] extends  Monad[T, F]{

  def :*:[P >: T, U](fs: F[P => U])(implicit h: ApplicativeHelper[F]): F[U] =  {
    flatMap(x => h(fs).map(f => f(x)))
  }

  def :@:[R, Q >: T](f: Q => R)  = map(f)
}

object Applicative {

  def liftA2[T, P, Q, A[_]](f: (T, P) => Q,  a1: A[T], a2:  A[P])(implicit h: ApplicativeHelper[A]): A[Q] =  {
      (f.curried :@: h(a1)) :*: h(a2)
    }

      def sequence[T, A[_]](input: List[A[T]])(implicit h: ApplicativeHelper[A]): A[List[T]] = {
        input match {
          case (x::xs) =>
            def cons(head: T, list: List[T]): List[T] = head::list
            liftA2(cons, x, sequence(xs))
          case _ => h(List.empty[T])
        }
    }

}



