package com.promindis.patterns


case class State[+T, S](f: (S) => (T,S)) {
  //newtype State s a = State { runState :: s -> (a, s) }
  def apply(s: S): (T, S) = f(s)
}

trait  StateM[T, S, M[_]] {

    def f: S => M[(T,S)]

    def apply(s: S): M[(T,S)] = f(s)
}


trait StateMonad[M[_], A] {

  def update(container: M[A])(f: A => A)(implicit m: Applicative[M]): M[A]

  def set(container: M[A])(data: A)(implicit m: Applicative[M]) = update(container){_ => data}

  def fetch(container: M[A])(implicit m: Applicative[M]): M[A] = update(container)(identity)
}
