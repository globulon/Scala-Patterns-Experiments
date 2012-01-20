package com.promindis.Logger

import com.promindis.patterns.Monoid


trait Writer[T, M] {
  val value: T
  val log: M
  def context: (T, M) = (value, log)
  def combined(otherLog: M )(implicit monoid: Monoid[M]) = monoid.add(log, otherLog)
}



