package com.promindis.patterns

trait ApplicativeHelper[F[_]] extends MonadHelper[F]{
  implicit def toApplicative[T](c: F[T]): Applicative[T, F]
}

trait Applicative[T, F[_]] extends Monad[T, F] {

    def :*:[R](fs: F[T => R])(implicit helper: ApplicativeHelper[F]): F[R] =
      flatMap(x => helper.toApplicative(fs).map(f => f(x)))

    def :@: [R](f: T=>R) = map(f)
}

object Applicative {
  def liftA2[T, P, Q, F[_]](f: (T , P) => Q, a1: F[T], a2: F[P])(implicit helper: ApplicativeHelper[F]) = {
    (f.curried :@: helper.toApplicative(a1)) :*: helper.toApplicative (a2)
  }

//  def selectedA[T, F[_]](list: List[F[T]])(implicit helper: ApplicativeHelper[F]): F[List[T]] = {
//    def cons[R](x: R, xs: List[R]) = x::xs
//
//    list match {
//      case (x::xs) => liftA2(cons, x, selectedA(xs))
//      //        case _
//    }
//  }
}




