package com.promindis.patterns

/**
 * Date: 21/02/12
 * Time: 20:05
 */

trait  Traverse[C[_]] {

  def  traverse[M[_]:Applicative, T, U](source: C[M[T]])(f: T => M[U]): M[C[U]]

  def dist[M[_], T](source: C[M[T]])(implicit application: M[T]): M[C[T]]

}