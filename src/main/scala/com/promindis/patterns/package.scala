package com.promindis

package object patterns {

  implicit def ToFunctor[F[_] : Functor, A](ma: F[A]) = new {
    val functor = implicitly[Functor[F]]
    final def map[B](f: A ⇒ B): F[B] = functor.map(ma)(f)
  }

  implicit def ToMonad[M[_]: Functor : Monad, T](source: M[T]) = new {
    val monad = implicitly[Monad[M]]
    def flatMap[U](f: T ⇒ M[U]): M[U] = monad.flatMap(source)(f)
  }

  implicit object ApplicativeList extends Applicative[List] with Monad[List] with Functor[List]{
    override def map[T, P>: T, U](source: List[T])(f: (P) ⇒ U) = source.map(f)

    override def apply[T](from: T) = List(from)

    override def flatten[T](m: List[List[T]]) = m.flatten
  }

  implicit object ApplicativeOption extends Applicative[Option] with Monad[Option] with Functor[Option]{
    override def map[T, P >: T, U](source: Option[T])(f: (P) ⇒ U) = source.map(f)

    override def apply[T](from: T) = Some(from)

    override def flatten[T](m: Option[Option[T]]) = m match {
      case Some(Some(data)) ⇒ Some(data)
      case _ ⇒ None
    }
  }

  case class BuilderToApplicative[T1, A[_]](m1: A[T1])(implicit applicative: Applicative[A]) {

    def :@:[T2](f: T1 ⇒ T2): A[T2] = applicative.mapA(f, m1)

    def :*:[T2](m2: A[T2]) = BuilderToApplicative2[T2](m2)

    case class BuilderToApplicative2[T2](m2: A[T2]) {
      def :@:[T3](f: (T1, T2) ⇒ T3) = Applicative.liftA2(f, m1, m2)

      def :*:[T3](m3: A[T3]) = BuilderToApplicative3[T3](m3)

      case class BuilderToApplicative3[T3](m3: A[T3]) {
        def :@:[T4](f: (T1, T2, T3) ⇒ T4): A[T4] = Applicative.liftA3(f, m1, m2, m3)
      }
    }
  }

  implicit def toApplicative[T, A[_]](m: A[T])(implicit applicative: Applicative[A]) =
    new BuilderToApplicative[T, A](m)

  implicit def toSequenceable[T, A[_]](list: List[A[T]])(implicit applicative: Applicative[A]) = new {
    def sequenceA = Applicative.sequence(list)
  }


  implicit def stateToComprehension[T, S](state: State[T,S]) = new {
    implicit val functor = stateFunctor[S]()
    val monad = stateMonad[S]()

    def map[U](f: T ⇒ U) = functor.map(state)(f)

    def flatMap[U](f: T ⇒ State[U, S]) = monad.flatMap(state)(f)
  }

  implicit def stateFunctor[S]() = new Functor[({type λ[α] = State[α,S]})#λ] {
    def map[T, P >: T, U](source: State[T, S])(f: P ⇒ U): State[U, S] = new State[U, S]((s: S) ⇒ {
      val (value, state) = source(s)
      (f(value), state)
    })
  }

  implicit def stateMonad[S]() =  new Monad[({type λ[α] = State[α,S]})#λ]{

    def apply[T](data: T) = new State((s: S) ⇒ (data, s))

    def flatten[T](m: State[State[T, S], S]): State[T, S] = new State[T, S]((s: S) ⇒ {
      val (mp, sp) = m(s)
      mp(sp)
    })
  }


  implicit def functionToComprehension[A, R](f: A ⇒ R) = new {
    implicit val applicative = FunctionApplicative[A]()

    def flatMap[T](g: (R) ⇒ (A) ⇒ T) = applicative.flatMap(f)(g)

    def map[T](g: R ⇒ T) = applicative.map(f)(g)

  }

  case class FunctionApplicative[A]() extends Applicative[({type λ[α] = Function[A,α]})#λ]{
      def apply[T](data: T) = ( _ ⇒ data)

      def flatten[T](m: (A) ⇒ (A) ⇒ T) = (x: A) ⇒ m(x)(x)

      def map[T, P >: T, U](source: (A) ⇒ T)(f: (P) ⇒ U) = f.compose(source)
  }

  implicit object ListTraverse extends Traverse[List] {
    private def cons[T](x: T, xs: List[T]): List[T]= x::xs

    def traverse[M[_], T, U](source: List[M[T]])(f: (T) ⇒ M[U])(implicit applicative: Applicative[M]): M[List[U]] = {
      source match {
        case (x::xs) ⇒
          val head: M[U] = applicative.flatMap(x)(f)
          lift(cons[U], head, traverse(xs)(f))
        case _ ⇒ applicative(Nil)
      }
    }
  }

}

