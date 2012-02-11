package com.promindis.user

import com.promindis.state._
import SM._

object Stack {
  def push[A](x: A) = new StateMonad[Unit, List[A]] {
    def apply(state: List[A]) = ((), x :: state)
  }

  def pop[A] = new StateMonad[Option[A], List[A]] {
    def apply(state: List[A]) =
      state match {
        case x :: xs => (Some(x), xs)
        case _ => (None, state)
      }
  }
}

object StackP {

  def pushP(x: Int) = new State((state: List[Int]) => ((), x :: state))

  def popP = new State((state: List[Int]) =>
    state match {
      case x :: xs => (Some(x), xs)
      case _ => (None, state)
    }
  )
}


object UseState {

  import Stack._
  import StackP._
  import SM._

  def main(args: Array[String]) {
    val resultp = for {
      _ <- pushP(3)
      _ <- pushP(5)
      _ <- pushP(7)
      _ <- pushP(9)
      _ <- popP

    } yield ()

    println(resultp(List(1))._2)

    val result = for {
      _ <- push(3)
      _ <- push(5)
      _ <- push(7)
      _ <- push(9)
      _ <- pop
    } yield ()
    println(result(List(1))._2)

    val otherResult = push(3).flatMap {
      _ =>
        push(5).flatMap {
          _ =>
            push(7).flatMap {
              _ =>
                push(9).flatMap {
                  _ =>
                    pop.map {
                      _ => ()
                    }
                }
            }
        }
    }

    println(otherResult(List(1))._2)
  }

}
