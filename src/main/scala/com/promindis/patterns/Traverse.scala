package com.promindis.patterns

/**
 * Date: 21/02/12
 * Time: 20:05
 */

trait Traverse[C[_]] {
  def traverse[M[_], T, U](source: C[M[T]])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M]): M[C[U]]

  def sequence[M[_] : Applicative, T](source: C[M[T]]): M[C[T]]
}

trait ListLike[C[_]] {
  def cons[T](x: T, xs: C[T]): C[T]

  def empty[T](): C[T]

  def first[M[_], T](source: C[M[T]]): Option[M[T]]

  def rest [M[_], T](source: C[M[T]]): C[M[T]]

}

trait TraverseLike[C[_]]{
  self : ListLike[C] =>

  def traverse[M[_]: Applicative, T, U](source: C[M[T]])(f: (T) ⇒ M[U]): M[C[U]] = {
    val applicative = implicitly[Applicative[M]]
    first(source) match {
      case Some(m) ⇒
        val head: M[U] = applicative.flatMap(m)(f)
        Applicative.liftA2(cons[U], head, traverse(rest(source))(f))
      case _ ⇒ applicative(empty[U]())
    }
  }


  def sequence[M[_] : Applicative, T](source: C[M[T]]): M[C[T]] = {
    val applicative = implicitly[Applicative[M]]
    traverse[M, T, T](source){x: T ⇒ applicative(x)}
  }

}

