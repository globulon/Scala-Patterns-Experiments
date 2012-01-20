package com.promindis.user

import com.promindis.state._

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

object UseState {
  import Stack._
  def main(args: Array[String]) {
    val result = for {
      _ <- push(3)
      _ <- push(5)
      _ <- push(7)
      _ <- push(9)
      _ <- pop
    } yield ()
    println(result(List(1))._2)

    val otherResult = push(3).flatMap{ _ =>
      push(5).flatMap{_ =>
        push(7).flatMap{_ =>
          push(9).flatMap{_ =>
            pop.map{_ => ()}
          }
        }
      }
    }

    println(otherResult(List(1))._2)
  }
}

object Adler32{
  def encode(c: Char) = new StateMonad[Char, (Int, Int)] {
    def apply(checksum: (Int, Int)) = {
      val a = checksum._1
      val b = checksum._2
      val aprime = (a + (c.toByte & 0xff)) % 65521
      (c, (aprime, aprime + b))
    }
  }

  def from(checksum: (Int, Int)) = new StateMonad[(Int, Int), Char] {
    def apply(c: Char) = {
      val a = checksum._1
      val b = checksum._2
      val aprime = (a + (c.toByte & 0xff)) % 65521
      ((aprime, aprime + b), c)
    }

  }
}
