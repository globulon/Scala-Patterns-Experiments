package com.promindis.state

import com.promindis.patterns.Monad


//trait StateMonad2[S, P[_, _] ,M[_] <: P [_, S]] extends Monad[M] {
//  owner =>
//
////  def
//}
//
//class SMonad extends StateMonad2[List[Int], Tuple2[_, _], Tuple2[_, List[Int]]] {
//  def apply[T](data: T) = (data, List[Int]())
//
//  def flatten[T](m: ((T, T2), T2)) = null
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

