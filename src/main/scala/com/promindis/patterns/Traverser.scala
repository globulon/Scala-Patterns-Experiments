package com.promindis.patterns

/**
 * Date: 21/02/12
 * Time: 20:05
 */

trait Traverser[C[_]] {
  def cons[T](x: T, xs: C[T]): C[T]

  def empty[T](): C[T]

  def first[M[_], T](source: C[M[T]]): Option[M[T]]

  def rest [M[_], T](source: C[M[T]]): C[M[T]]
}

object Traverser {

  def traverse[M[_], C[_], T, U](source: C[M[T]])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M], t: Traverser[C]): M[C[U]] = {
    t.first(source) match {
      case Some(m) ⇒
        val head: M[U] = applicative.flatMap(m)(f)
        Applicative.liftA2(t.cons[U], head, traverse(t.rest(source))(f))
      case _ ⇒ applicative(t.empty[U]())
    }
  }


  def sequence[M[_] : Applicative, C[_]: Traverser, T](source: C[M[T]]): M[C[T]] = {
    val applicative = implicitly[Applicative[M]]
    traverse[M, C, T, T](source){x: T ⇒ applicative(x)}
  }

}

