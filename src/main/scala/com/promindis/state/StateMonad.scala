package com.promindis.state

import com.promindis.patterns.Monad




trait StateMonad2[S, P[_, _] ,M[X] <: P [X, S]] extends Monad[M] {
  owner =>

//  def
}

//class StateMonad2C[T] extends (T, List[Int])
//
//class SMonad extends StateMonad2[List[Int], Tuple2, StateMonad2C] {
//  def apply[T](data: T) = null
//
//  def flatten[T](m: StateMonad2C[StateMonad2C[T]]) = null
//}

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

