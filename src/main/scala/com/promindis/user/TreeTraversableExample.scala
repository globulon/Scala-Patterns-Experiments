package com.promindis.user

import com.promindis.patterns._


/**
 * Date: 28/02/12
 * Time: 17:08
 */

sealed trait Tree[+A]

final case class Node[A](a: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]

object Leaf extends Tree[Nothing]

object TraversableTree extends Traverse[Tree] {
  def cons[T](v: T, l: Tree[T], r: Tree[T]): Tree[T] = Node(v, l, r)

  def traverse[M[_], T, U](source: Tree[T])(f: (T) => M[U])(implicit applicative: Applicative[M]): M[Tree[U]] = {
    source match {
      case Leaf => applicative(Leaf)
      case Node(v, left, right) =>
        cons[U] _ :@:f(v):*:traverse(left)(f):*:traverse(right)(f)

    }
  }
}

object TreeTraversableExample {

  val aTree: Tree[Int] = Node(5, Node(3, Node(1), Node(6)), Node(9, Node(8), Node(10)))

  def cons[T](v: T, l: Tree[T], r: Tree[T]): Tree[T] = Node(v, l, r)

  def newState[X, U](x: X, f: X => U, monoid: Monoid[U]) = State((acc: U) => ((), monoid.add(f(x), acc)))

//    def accumulate[A, T[_] : Traverse, U: Monoid](source: T[A])(f: (A) => U) = {
//    val t = implicitly[Traverse[T]]
//    val monoid = implicitly[Monoid[U]]
//
//    t.traverse[({type λ[U] = State[Unit, U]})#λ, A, U](source)((x: A) => newState(x, f, monoid))
//  }


  def main(args: Array[String]) {

  }


}
