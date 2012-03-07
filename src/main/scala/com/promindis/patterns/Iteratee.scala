package com.promindis.patterns

/**
 * Date: 07/03/12
 * Time: 08:44
 */

object Iteratee {

  sealed trait StreamG[-E]

  final case class Element[E](el: E) extends StreamG[E]
  case object EOF extends StreamG[Any]
  case object EMPTY extends StreamG[Any]

  sealed trait IterV[-E, +A]
  final case class Done[E, A](a: A, s: StreamG[E]) extends IterV[E, A]
  final case class Cont[E, A](f: StreamG[E] => IterV[E, A]) extends IterV[E, A] {
    def apply[F >: E](s: StreamG[E]) = f(s)
  }

  def enum[E, A](i: IterV[E, A], el: Seq[E]): IterV[E, A] = {
    (i, el) match {
      case _ if el.isEmpty => i
      case (Done(_, _), _) => i
      case (c@Cont(_), (e :: es)) => enum(c(Element(e)), es)
    }
  }

  def run[E, A](i: IterV[E, A]): Option[A] = {
    def iterRun(next: IterV[E, A]) = next match {
      case Done(a, _) => Some(a)
      case _ => None
    }

    i match {
      case Done(a, _) => Some(a)
      case c@Cont(_) => iterRun(c(EOF))
    }

    //TODO should handle a default case
  }

}
