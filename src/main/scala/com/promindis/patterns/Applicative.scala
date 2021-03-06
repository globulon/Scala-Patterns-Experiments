package com.promindis.patterns

trait Applicative[F[_]] extends Functor[F] with Monad[F]{
  self  ⇒

  def pure[T](data: T) = apply[T](data: T)

  def applyA[T, U](fs: F[T ⇒ U])(source: F[T]): F[U] = {
    implicit val f: Functor[F] = self
    flatMap(fs) { f =>
      map(source) { x => f(x)}}
  }

  def mapA[T, U](t: T ⇒ U, source: F[T]): F[U] = {
    applyA(pure(t))(source)
  }

}

object Applicative {

  def liftA2[T, P, Q, A[_]](f: (T, P) ⇒ Q, a1: A[T], a2: A[P])(implicit applicative: Applicative[A]): A[Q] = {
    import applicative._
    applyA(mapA(f.curried, a1))(a2)
  }

  def liftA3[T, P, Q, R, A[_]](f: (T, P, Q) ⇒ R, a1: A[T], a2: A[P], a3:A[Q])(implicit applicative: Applicative[A]): A[R] = {
    import applicative._
    applyA(applyA(mapA(f.curried, a1))(a2))(a3)
  }

  def sequence[T, A[_]](input: List[A[T]])(implicit applicative: Applicative[A]): A[List[T]] = {
    import applicative._
    input match {
      case (x :: xs) ⇒
        def cons(head: T, list: List[T]): List[T] = head :: list
        liftA2(cons, x, sequence(xs))
      case _ ⇒ pure(List.empty[T])
    }
  }
}



