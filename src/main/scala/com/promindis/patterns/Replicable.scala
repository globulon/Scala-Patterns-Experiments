package com.promindis.patterns

trait Replicable[T] {
  def replicate[L[_]](value: T, n: Int)(implicit m: MonoidC[L]): L[T] = {
    def replicateIter(acc: L[T], rest: Int): L[T] = {
      if (rest == 0) acc
      else replicateIter(m.add(acc, m(value)), rest - 1)
    }

    replicateIter(m.unit, n)
  }
}



