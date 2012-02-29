package com.promindis.user

import com.promindis.patterns.{FunctionApplicative, Monoid, Applicative, Traverse}


/**
 * Date: 28/02/12
 * Time: 17:08
 */

sealed trait Tree[+A]
final case class Node[A](a: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]
object Leaf extends Tree[Nothing]

object TraversableTree extends Traverse[Tree] {
  def cons[T](v: T, l: Tree[T], r: Tree[T]) : Tree[T] = Node(v, l, r)

  def traverse[M[_]: Applicative, T, U](source: Tree[T])(f: (T) => M[U]): M[Tree[U]] = {
    val applicative = implicitly[Applicative[M]]
    source match {
      case Leaf => applicative(Leaf)
      case Node(v, left, right) =>
        Applicative.liftA3(cons[U], f(v), traverse(left)(f), traverse(right)(f))

    }
  }
}

object TreeTraversableExample {

  val aTree: Tree[Int] = Node(5, Node(3, Node(1),Node(6)), Node(9, Node(8), Node(10)))

//  def accumulate[T, TR[_]: Traverse, O: Monoid](f: T => Monoid[O])(t: TR[T]): Monoid[O]  ={
//    val traversable = implicitly[Traverse[TR]]
//
//
//    traversable.traverse(t, f)
//  }

  def main(args: Array[String]) {

  }
}
