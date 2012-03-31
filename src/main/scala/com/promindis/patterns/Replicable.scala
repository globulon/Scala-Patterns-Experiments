package com.promindis.patterns

final case class  Replicable[T](value: T) {
  def replicate[L[_]](n: Int)(implicit m: MonoidC[L]): L[T] = {
    def replicateIter(acc: L[T], rest: Int): L[T] = {
      if (rest == 0) acc
      else replicateIter(m.add(acc, m(value)), rest - 1)
    }
    replicateIter(m.unit, n)
  }
}

final case class  ReplicableM[T, M[_]](value: M[T]) {
  def replicate[L[_]](n: Int)(implicit m: MonoidC[L]): L[M[T]] = {
    def replicateIter(acc: L[M[T]], rest: Int): L[M[T]] = {
      if (rest == 0) acc
      else replicateIter(m.add(acc, m(value)), rest - 1)
    }
    replicateIter(m.unit, n)
  }

}





