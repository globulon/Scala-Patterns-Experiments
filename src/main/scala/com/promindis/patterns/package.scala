package com.promindis

package object patterns {

  implicit def ToFunctor[F[_] : Functor, A](ma: F[A]) = new {
    val functor = implicitly[Functor[F]]
    final def map[B](f: A => B): F[B] = functor.map(ma)(f)
  }

  implicit def ToMonad[M[_]: Functor : Monad, T](source: M[T]) = new {
    val monad = implicitly[Monad[M]]
    def flatMap[U](f: T => M[U]): M[U] = monad.flatMap(source)(f)
  }

  implicit object ApplicativeList extends Applicative[List] with Monad[List] with Functor[List]{
    override def map[T, U](source: List[T])(f: (T) => U) = source.map(f)

    def apply[T](from: T) = List(from)

    def flatten[T](m: List[List[T]]) = m.flatten
  }

  implicit object ApplicativeOption extends Applicative[Option] with Monad[Option] with Functor[Option]{
    override def map[T, U](source: Option[T])(f: (T) => U) = source.map(f)

    def apply[T](from: T) = Some(from)

    def flatten[T](m: Option[Option[T]]) = m match {
      case Some(Some(data)) => Some(data)
      case _ => None
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

  implicit def toSequenceable[T, A[_]](list: List[A[T]])(implicit applicative: Applicative[A]) = new {
    def sequenceA = Applicative.sequence(list)
  }
}

