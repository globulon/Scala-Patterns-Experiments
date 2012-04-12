package com.promindis.patterns


trait StateMonad[M[_]]{

  def update[S](f: S => S)(implicit m: Monad[M]): M[S]

  def set[S](s: S)(implicit m: Monad[M]) = update((ignored: S) => s)

  def fetch[S](implicit m: Monad[M]): M[S] = update(identity)


}
