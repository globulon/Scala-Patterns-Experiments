package com.promindis.user

import com.promindis.patterns._


/**
 * Date: 28/02/12
 * Time: 17:08
 */


object TraversableExample {
  sealed trait Tree[+A]
  final case class Node[A](a: A, left: Tree[A] = Leaf, right: Tree[A] = Leaf) extends Tree[A]
  case object Leaf extends Tree[Nothing]

  implicit object traversableTree extends Traverse[Tree] {
    def cons[T](v: T, l: Tree[T], r: Tree[T]): Tree[T] = Node(v, l, r)

    def traverse[M[_], T, U](source: Tree[T])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M]): M[Tree[U]] = {
      source match {
        case Leaf ⇒ applicative(Leaf)
        case Node(v, left, right) ⇒
          cons[U] _ :@: f(v) :*: traverse(left)(f) :*: traverse(right)(f)
      }
    }
  }

  def main(args: Array[String]) {
    println(traversableList.sequence(List[Option[Int]](Some(1), Some(2), Some(3))))
    println(traversableList.sequence(List[Option[Int]](Some(1), None, Some(3))))
    println(traversableTree.sequence(Node[Option[Int]](Some(5), Node(Some(3), Leaf, Node(Some(2))))))
    println(traversableTree.sequence(Node[Option[Int]](Some(5), Node(Some(3), Leaf, Node(None)))))

    val aTree: Tree[Int] = Node(5, Node(3, Node(1), Node(6)), Node(9, Node(8), Node(10)))
    val result = accumulate(aTree)((x: Int) ⇒ (x > 8))(traversableTree, Any)
    println(result(Any.unit)._2)
    val result2 = accumulate(aTree)((x: Int) ⇒ if (x > 8) List(x) else Nil)
    println(result2(toListMonoid.unit)._2)

    println((collect(List(10, 20, 30, 40))((a: Int) ⇒ 2 * a, (i: Int) ⇒ i + 1)).apply(0))

    println(reduce(List("Thumper", "is", "a", "cute", "rabbit")))
  }


}
