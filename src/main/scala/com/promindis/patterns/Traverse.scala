package com.promindis.patterns

/**
 * Date: 21/02/12
 * Time: 20:05
 */

trait  Traverse[C[_]] {

  def lift[T, P, Q, A[_]](f: (T, P) ⇒ Q, a1: A[T], a2: A[P])(implicit applicative: Applicative[A]): A[Q] = {
    import applicative._
    applyA(mapA(f.curried, a1))(a2)
  }

  def  traverse[M[_], T, U](source: C[M[T]])(f: T ⇒ M[U])(implicit applicative: Applicative[M]): M[C[U]]

  def dist[M[_], T](source: C[M[T]])(implicit applicative: Applicative[M]): M[C[T]] =
    traverse[M, T, T](source){x: T ⇒ applicative(x)}

}

