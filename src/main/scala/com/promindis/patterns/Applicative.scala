package com.promindis.patterns

trait ApplicativeHelper[F[_]] extends MonadHelper[F] {
  implicit def unit[U](data: U): F[U]

  implicit def toApplicative[T](list: F[T]): Applicative[T, F]
}

trait Applicative[T, F[_]] extends  Monad[T, F]{
  implicit val applicativeHelper : ApplicativeHelper[F]
  import applicativeHelper._

  def :*:[U](fs: F[T => U]): F[U] =  {
     flatMap(x => fs.map(f => f(x)))
  }

  def :@:[R](f: T => R)  = map(f)
}

object Applicative {

  def liftA2[T, P, Q, A[_]](f: (T, P) => Q,  a1: A[T], a2:  A[P]) (implicit h: ApplicativeHelper[A]): A[Q] =  {
    import h._
    (f.curried :@: a1) :*: a2
  }

  def sequence[T, A[_]](input: List[A[T]])(implicit h: ApplicativeHelper[A]): A[List[T]] = {
    import h._
    input match {
      case (x::xs) =>
        def cons(head: T, list: List[T]): List[T] = head::list
        liftA2(cons, x, sequence(xs))
      case _ => List.empty[T]
    }
  }

}



