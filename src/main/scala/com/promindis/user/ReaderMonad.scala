package com.promindis.user
import com.promindis.patterns._
/**
 * Date: 21/02/12
 * Time: 22:27
 */

object ReaderMonad {
  def `*2` = (x: Int) ⇒ x * 2
  def `+10` = (x: Int) ⇒ 10 + x

  def extract(name: String)(fromMap: Map[String, String]) =
    fromMap.get(name)

  def extract2(name: String)(fromMap: Map[String, String]) =
    fromMap.get(name)

  val fittingProperties = Map (
    "login" → "thumper",
    "password" → "thumpthump",
    "url" → "jdbc:forest"
  )

  val invalidProperties = Map (
    "login" → "thumper",
    "url" → "jdbc:forest"
  )

  def configBuilder = for {
    user ← extract("login") _
    password ← extract("password") _
    url ← extract2("url") _
  } yield (List(user, password, url))

  def main(args: Array[String]) {
    println((for {
      a ← `*2`
      b ←  `+10`
    } yield (a + b))(3))

    println(configBuilder(fittingProperties))
    println(configBuilder(invalidProperties).sequenceA)


  }
}
