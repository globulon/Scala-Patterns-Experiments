package com.promindis.patterns

/**
 * Date: 12/02/12
 * Time: 17:08
 */

case class State[T, S](f: (S) => (T,S)) {
  def apply(s: S): (T, S) = f(s)
}