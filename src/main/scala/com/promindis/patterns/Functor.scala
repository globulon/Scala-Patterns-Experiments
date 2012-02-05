package com.promindis.patterns

trait Functor[M[_]] {

  def map[T, U](source: M[T])(f: T => U): M[U];
}

object Functor {
  implicit def functorOps[F[_] : Functor, A](ma: F[A]) = new {
    val functor = implicitly[Functor[F]]
    final def map[B](f: A => B): F[B] = functor.map(ma)(f)
  }
}