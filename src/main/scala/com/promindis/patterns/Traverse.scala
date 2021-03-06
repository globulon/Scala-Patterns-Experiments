package com.promindis.patterns

/**
 * Date: 21/02/12
 * Time: 20:05
 */

trait Traverse[C[_]] {
  def traverse[M[_], T, U](source: C[T])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M]): M[C[U]]

  final def sequence[M[_], T](source: C[M[T]])(implicit applicative: Applicative[M]): M[C[T]] = {
    traverse[M, M[T], T](source){identity}
  }
}

trait ListLike[C[_]] {
  def cons[T](x: T, xs: C[T]): C[T]

  def empty[T](): C[T]

  def first[T](source: C[T]): Option[T]

  def rest [T](source: C[T]): C[T]

}

trait TraverseListLike[C[_]] extends Traverse[C]{
  self : ListLike[C] =>

  override def traverse[M[_], T, U](source: C[T])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M]): M[C[U]] = {
    first(source) match {
      case Some(value) ⇒
        cons[U] _ :@:f(value):*:traverse(rest(source))(f)
      case _ ⇒ applicative(empty[U]())
    }
  }
}

