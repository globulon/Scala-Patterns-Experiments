package com.promindis.patterns

trait Applicative[F[_]] {
  self: Functor[F] with Monad[F] =>


  def pure[T](data: T) = apply[T](data: T)

  def <*>[T, U](fs: F[T => U])(source: F[T]): F[U] = {
    implicit val f: Functor[F] = self
    flatMap(source) {
      x: T => map(fs)(f => f(x))
    }
  }

  def <@>[T, U](t: T => U, source: F[T]): F[U] = {
    <*>(pure(t))(source)
  }
}

object Applicative {

  def lift[T, P, Q, A[_]](f: (T, P) => Q, a1: A[T], a2: A[P])(implicit applicative: Applicative[A]): A[Q] = {
    import applicative._
    <*>(<@>(f.curried, a1))(a2)
  }

  def sequence[T, A[_]](input: List[A[T]])(implicit applicative: Applicative[A]): A[List[T]] = {
    input match {
      case (x :: xs) =>
        def cons(head: T, list: List[T]): List[T] = head :: list
        lift(cons, x, sequence(xs))
      case _ => applicative.pure(List.empty[T])
    }
  }

  case class BuilderToApplicative[T, A[_]](m: A[T])(implicit applicative: Applicative[A]) {
    def :*:[U](fs: A[T => U]): A[U] = {
      applicative.<*>(fs)(m)
    }

    def :@:[U](f: T => U): A[U] = applicative.<@>(f, m)
  }

  implicit def toApplicative[T, A[_]](m: A[T])(implicit applicative: Applicative[A]) =
    new BuilderToApplicative[T, A](m)
}



