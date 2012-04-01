package com.promindis.patterns

import annotation.tailrec


sealed trait StreamG[+E]

final case class Element[E](data: E) extends StreamG[E]
case object EOF extends StreamG[Nothing]
case object EMPTY extends StreamG[Nothing]

sealed trait IterV[-E, +A]
case class Done[E, A](value: A, stream: StreamG[E]) extends IterV[E, A]
case class Cont[E, A](f: StreamG[E] => IterV[E, A]) extends IterV[E, A] {
  def apply[F >: E](s: StreamG[E]) = f(s)
}

object Iteratee {

  implicit def iterateesToApplicative[E]() = new Applicative[({type λ[α] = IterV[E, α]})#λ] {
    def apply[T](data: T) = {
      Done(data, EMPTY)
    }

    def flatten[T](source: IterV[E, IterV[E, T]]): IterV[E, T] = source match {
      case Done(Done(v, _), s) => Done(v, s)
      case Done(Cont(f), s) => f(s)
      case Cont(f) => Cont[E, T]{
        s: StreamG[E] => flatten(f(s))
      }
    }

    def map[T, P >: T, U](source: IterV[E, T])(f: (P) => U): IterV[E, U] =  source match {
      case Done(t, s) => Done(f(t), s)
      case Cont(g) => Cont[E, U] {
        stream: StreamG[E] =>  map(g(stream))(f)
      }
    }
  }

  implicit def iterateeOnSteroid[E,A](iter: IterV[E, A]) = new {
    implicit val applicative = iterateesToApplicative[E]()

    def map[B >: A, C](f: B => C) = applicative.map(iter)(f)

    def flatMap[B >: A, C](f: B => IterV[E, C]) = applicative.flatMap(iter)(f)
  }

  @tailrec
  def enum[E, A](iter: IterV[E, A], el: Seq[E]): IterV[E, A] = {
    (iter, el) match {
      case _ if el.isEmpty => iter
      case (Done(_, _), _) => iter
      case (c@Cont(_), (e :: es)) => enum(c(Element(e)), es)
    }
  }

  def run[E, A](iter: IterV[E, A]): Option[A] = {
    def iterRun(next: IterV[E, A]) = next match {
      case Done(a, _) => Some(a)
      case _ => None
    }

    iter match {
      case Done(a, _) => Some(a)
      case c@Cont(_) => iterRun(c(EOF))
    }
  }

  def head[E]: IterV[E, Option[E]] = Cont[E, Option[E]] {
    s: StreamG[E] =>
      s match {
        case Element(e) => Done(Some(e), EMPTY)
        case EMPTY => head[E]
        case EOF => Done(None, EOF)
      }
    }

  def first[E]: IterV[E, Option[E]] = Cont[E, Option[E]] {
    s: StreamG[E] =>
      s match {
        case Element(e) => Done(Some(e), s)
        case EMPTY => first[E]
        case EOF => Done(None, EOF)
      }
  }


  def drop[E](n: Int): IterV[E, Unit] = {
    assert (n >= 0)
    def dropCont() = Cont[E, Unit]{
      s: StreamG[E] =>
        s match {
          case Element(e) => drop(n - 1)
          case EMPTY => drop(n)
          case EOF => Done((), EOF)
        }
    }

    if (n == 0) Done((), EMPTY) else dropCont()
  }

  def length[E]: IterV[E, Int] = {
    def length(acc: Int): IterV[E, Int] = Cont[E, Int]{
      s: StreamG[E] =>
        s match {
          case Element(_) => length(acc + 1)
          case EMPTY => length(acc)
          case EOF => Done(acc, EOF)
        }
    }
    length(0)
  }


}
