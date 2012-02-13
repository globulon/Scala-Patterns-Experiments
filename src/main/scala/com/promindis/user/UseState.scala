package com.promindis.user

import com.promindis.patterns._



object Stack {

  def push[Int](x: Int) = new State((state: List[Int]) => ((), x :: state))

  def pop = new State((state: List[Int]) =>
    state match {
      case x :: xs => (Some(x), xs)
      case _ => (None, state)
    }
  )
}


object UseState {
  import Stack._

  def main(args: Array[String]) {
    val resultp = for {
      _ <- push(3)
      _ <- push(5)
      _ <- push(7)
      _ <- push(9)
      _ <- pop

    } yield ()

    println(resultp(List(1))._2)

    val otherResult =
      push(3).flatMap{ _ =>
        push(5).flatMap{ _ =>
          push(7).flatMap{ _ =>
            push(9).flatMap { _ =>
              pop.map { _ => ()}
            }
          }
        }
    }

    println(otherResult(List(1))._2)
  }

}
