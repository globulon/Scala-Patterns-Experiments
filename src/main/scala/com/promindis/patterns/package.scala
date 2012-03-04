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
      def :@:[T3](f: (T2, T1) ⇒ T3) = Applicative.liftA2(f, m2, m1)

      def :*:[T3](m3: A[T3]) = BuilderToApplicative3[T3](m3)

      case class BuilderToApplicative3[T3](m3: A[T3]) {
        def :@:[T4](f: (T3, T2, T1) ⇒ T4): A[T4] = Applicative.liftA3(f, m3, m2, m1)
      }
    }
  }

  implicit def toApplicative[T, A[_]](m: A[T])(implicit applicative: Applicative[A]) =
    new BuilderToApplicative[T, A](m)

  implicit def toSequenceable[T, A[_]](list: List[A[T]])(implicit applicative: Applicative[A]) = new {
    def sequenceA = Applicative.sequence(list)
  }


  implicit def stateToComprehension[T, S](state: State[T,S]) = new {
    implicit val applicative = stateApplicative[S]()

    def map[U](f: T ⇒ U) = applicative .map(state)(f)

    def flatMap[U](f: T ⇒ State[U, S]) = applicative.flatMap(state)(f)
  }

  implicit def stateApplicative[S]() = new Applicative[({type λ[α] = State[α,S]})#λ] {
    def map[T, P >: T, U](source: State[T, S])(f: P ⇒ U): State[U, S] = new State[U, S]((s: S) ⇒ {
      val (value, state) = source(s)
      (f(value), state)
    })

    def apply[T](data: T) = new State((s: S) ⇒ (data, s))

    def flatten[T](m: State[State[T, S], S]): State[T, S] = new State[T, S]((s: S) ⇒ {
      val (mp, sp) = m(s)
      mp(sp)
    })
  }

  implicit def functionToComprehension[A, R](f: A ⇒ R) = new {
    implicit val applicative = FunctionApplicative[A]()

    def flatMap[T](g: (R) ⇒ (A) ⇒ T): ((A)⇒ T) = applicative.flatMap(f)(g)

    def map[T](g: R ⇒ T) = applicative.map(f)(g)

  }

  case class FunctionApplicative[A]() extends Applicative[({type λ[α] = Function[A,α]})#λ]{
      def apply[T](data: T): (A) ⇒ T = ( _ ⇒ data)

      def flatten[T](m: (A) ⇒ (A) ⇒ T): (A) ⇒ T = (x: A) ⇒ m(x)(x)

      def map[T, P >: T, U](source: (A) ⇒ T)(f: (P) ⇒ U): (A)⇒ U  = f.compose(source)
  }

  def iff [M[_]: Monad: Functor, T](mb: M[Boolean], mt: ⇒ M[T], me: ⇒ M[T]): M[T] = {
    for {
      b ← mb
      r ← if (b) mt else me
    } yield r
  }

  implicit object StringMonoid extends Monoid[String] {
    override def add(x: String, y: String) = x + y
    override def unit = ""
  }

  implicit def toListMonoid[A] = new Monoid[List[A]] {
    def add(x: List[A], y: List[A]) = x ++ y

    def unit = List[A]()
  }

  object Any extends Monoid[Boolean] {
    def add(x: Boolean, y: Boolean) = x || y

    def unit = false
  }

  object All extends Monoid[Boolean] {
    def add(x: Boolean, y: Boolean) = x && y

    def unit = true
  }

  def toAccumulator[X, U](x: X, f: (X) ⇒ U, monoid: Monoid[U]) = State[X,U]((s: U) ⇒ (x, monoid.add(f(x), s)))

  def accumulate[A, T[_], U](source: T[A])(f: (A) ⇒ U)(implicit t: Traverse[T], monoid: Monoid[U]): State[T[A], U] = {
    type Projection[X] = State[X, U]
    implicit val A = stateApplicative[U]()
    t.traverse[Projection, A, A](source)((x: A) ⇒ toAccumulator(x, f, monoid))
  }

  def reduce[A, T[_]](source: T[A])(implicit t: Traverse[T], monoid: Monoid[A])=
    accumulate(source)(identity).apply(monoid.unit)

  def toCollector[A, B, U](x: A, f: (A) ⇒ B, g: U ⇒ U) = State[B,U]((s: U) ⇒ (f(x), g(s)))

  def collect[A, B, T[_], U](source: T[A])(f: (A) ⇒ B, g: U ⇒ U)(implicit t: Traverse[T]): State[T[B], U] = {
    type Projection[X] = State[X, U]
    implicit val A = stateApplicative[U]()
    t.traverse[Projection, A, B](source)((x: A) ⇒ toCollector(x, f, g))
  }

  implicit object ListTraverse extends TraverseListLike[List] with ListLike[List]{

    def empty[T]() = Nil

    def cons[T](x: T, xs: List[T]) = x::xs

    def first[T](source: List[T]) = source.headOption

    def rest[T](source: List[T]) = source.tail
  }
}

