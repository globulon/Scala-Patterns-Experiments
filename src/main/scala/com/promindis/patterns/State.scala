package com.promindis.patterns

/**
 * Date: 12/02/12
 * Time: 17:08
 */

final case class State[+T, S](f: (S) => (T,S)) {
  //newtype State s a = State { runState :: s -> (a, s) }
  def apply(s: S): (T, S) = f(s)
}