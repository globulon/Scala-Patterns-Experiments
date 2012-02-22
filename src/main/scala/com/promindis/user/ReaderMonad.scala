package com.promindis.user
import com.promindis.patterns._
/**
 * Date: 21/02/12
 * Time: 22:27
 */

object ReaderMonad {
  def `*2` = (x: Int) => x * 2
  def `+10` = (x: Int) => 10 + x

  def extract[String](name: String)(fromMap: Map[String, String]) =
    fromMap.get(name)

  def main(args: Array[String]) {
    println((for {
      a <- `*2`
      b <-  `+10`
    } yield (a + b))(3))

    val fittingProperties = Map (
      "login" → "thumper",
      "password" → "thumpthump",
      "url" → "jdbc:forest"
    )

    def configBuilder[T, M[_, _]](extract: String ⇒ M[String, T] ⇒ Option[T]) = for {
      user ← extract("login")_
      password ← extract("password")_
      url ← extract("url")_
    } yield (List(user, password, url))

    val config = configBuilder(extract)(fittingProperties)
    println(config.sequenceA)

  }
}
