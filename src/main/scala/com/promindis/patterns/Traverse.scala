package com.promindis.patterns

/**
 * Date: 21/02/12
 * Time: 20:05
 */

trait  Traverse[C[_]] {

  def cons[T](x: T, xs: C[T]): C[T]

  def empty[T](): C[T]

  def first[M[_], T](source: C[M[T]]): Option[M[T]]

  def rest [M[_], T](source: C[M[T]]): C[M[T]]


  def traverse[M[_], T, U](source: C[M[T]])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M]): M[C[U]] = {
    first(source) match {
      case Some(m) ⇒
        val head: M[U] = applicative.flatMap(m)(f)
        Applicative.liftA2(cons[U], head, traverse(rest(source))(f))
      case _ ⇒ applicative(empty[U]())
    }
  }


  def dist[M[_], T](source: C[M[T]])(implicit applicative: Applicative[M]): M[C[T]] =
    traverse[M, T, T](source){x: T ⇒ applicative(x)}

}

