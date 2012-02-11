package com.promindis.state

import com.promindis.patterns.{Functor, Monad}


object SM {
  case class State[T, S](f: (S) => (T,S)) {
    def apply(s: S): (T, S) = f(s)
  }

implicit def stateToComprehension[T, S](state: State[T,S]) = new {
  implicit val functor = stateFunctor[S]()
  val monad = stateMonad[S]()

  def map[U](f: T => U) = functor.map(state)(f)

  def flatMap[U](f: T => State[U, S]) = monad.flatMap(state)(f)
}

implicit def stateFunctor[S]() = new Functor[({type λ[α] = State[α,S]})#λ] {
  def map[T, U](source: State[T, S])(f: T => U): State[U, S] = new State[U, S]((s: S) => {
    val (value, state) = source(s)
    (f(value), state)
  })
}

implicit def stateMonad[S]() =  new Monad[({type λ[α] = State[α,S]})#λ]{

    def apply[T](data: T) = new State((s: S) => (data, s))

    def flatten[T](m: State[State[T, S], S]): State[T, S] = new State[T, S]((s: S) => {
      val (mp, sp) = m(s)
      mp(sp)
    })
  }
}


trait StateMonad[+T, S]  {
  owner =>
  def apply(state: S): (T, S)

  def flatMap[U](f: T => StateMonad[U,S]) = new StateMonad[U, S] {
    override def apply(state: S) = {
      val (a, y) =  owner(state)
      f(a)(y)
    }
  }

  def map[U](f: T => U) = new StateMonad[U, S] {
    def apply(state: S) = {
      val (a, y) =  owner(state)
      (f(a), y)
    }
  }
}

object StateMonad {
  def apply[T, S](value: T) = new StateMonad[T, S] {
    def apply(state: S) = (value, state)
  }
}

