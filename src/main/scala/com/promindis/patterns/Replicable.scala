package com.promindis.patterns

final case class  Replicable[T]() {
  def replicate[L[_]](value: T, n: Int)(implicit m: MonoidC[L]): L[T] = {
    def replicateIter(acc: L[T], rest: Int): L[T] = {
      if (rest == 0) acc
      else replicateIter(m.add(acc, m(value)), rest - 1)
    }
    replicateIter(m.unit, n)
  }

  def replicateC[L[_], M[_]](value: M[T], n: Int)(implicit m: MonoidC[L]): L[M[T]] = {
    def replicateIter(acc: L[M[T]], rest: Int): L[M[T]] = {
      if (rest == 0) acc
      else replicateIter(m.add(acc, m(value)), rest - 1)
    }
    replicateIter(m.unit, n)
  }

}

object Replicable {
  def replicate[T, L[_]: MonoidC](value: T, n: Int) = Replicable[T]().replicate(value, n)

  def replicateC[T, L[_]: MonoidC, M[_]](value: M[T], n: Int) = Replicable[T]().replicateC(value, n)
}




