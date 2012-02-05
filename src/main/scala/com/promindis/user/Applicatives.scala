package com.promindis.user

import com.promindis.patterns.{Functor, Monad, Applicative}

object Applicatives {

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
}
