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

  val properties = Map (
    "login" → "thumper",
    "password" → "thumpthump",
    "url" → "jdbc:forest"
  )

  case class Config[T](data: T)

  implicit object ApplicativeConfig extends Applicative[Config] {
    def apply[T](data: T) = Config(data)

    def flatten[T](m: Config[Config[T]]) = m.data

    def map[T, P >: T, U](source: Config[T])(f: (P) => U) = Config(f(source.data))
  }

  case class Connection()
  case class Datastore()
  case class Application()
  case class Environment(datastore: Datastore, application: Application)

  def connection(map: Map[String, String]): Config[Connection] = Config(Connection())
  def datastore(conn: Config[Connection])(map: Map[String, String]): Config[Datastore] = Config(Datastore())
  def application(map: Map[String, String]): Config[Application] = Config(Application())

  def build = (a: Application, d:  Datastore) => Environment(d, a)

  def configBuilder = for {
    conn ← connection _
    store ← datastore(conn) _
    app ← application _
  } yield (build:@:app:*:store)



  def main(args: Array[String]) {
    println((for {
      a ← `*2`
      b ←  `+10`
    } yield (a + b))(3))

    println(configBuilder(properties))
  }
}
