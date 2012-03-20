package com.promindis.patterns

trait Replicable[T] {
  def replicate[L[_]](value: T, n: Int)(implicit m: MonoidC[L]): L[T] = {
    def replicateIter(acc: L[T], rest: Int): L[T] = {
      if (rest == 0) acc
      else replicateIter(m.add(m(value), acc), rest - 1)
    }

    replicateIter(m.unit, n)
  }

  def replicateM[L[_], M[_]](value: M[T], n: Int)(implicit m: MonoidC[L]): L[M[T]] = {
    def replicateIter(acc: L[M[T]], rest: Int): L[M[T]] = {
      if (rest == 0) acc
      else replicateIter(m.add(m(value), acc), rest - 1)
    }

    replicateIter(m.unit, n)
  }

}




