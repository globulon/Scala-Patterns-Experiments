package com.promindis.patterns


trait Monoid[T] {
  def add(x: T, y: T): T

  def unit: T
}

trait Acc[A, T] {

  def m: Monoid[T]

  def apply(x: A): T
}

//class ApplicativeMonoid[M[_] <: Monoid]() extends Applicative[M]{
//
//  def apply[T](data: T) =
//
//  def flatten[T](m: M[M[T]]) = null
//
//  def map[T, P >: T, U](source: M[T])(f: (P) => U) = null
//}


//object Monoid {
//  case class Acc[A,T](f: A => T)(m: Monoid[T])
//
//  def ApplicativeAcc[A]() =  new Applicative[({type λ[α] = Acc[A,α]})#λ] {
//    def apply[T](data: T) = null
//
//    def flatten[T](m: ({type λ[α] = Acc[A, α]})#λ[({type λ[α] = Acc[A, α]})#λ[T]]) = null
//
//    def map[T, P >: T, U](source: ({type λ[α] = Acc[A, α]})#λ[T])(f: (P) => U) = null
//  }
//
//  def lift[A, T](f: (A) => T)(m: Monoid[T]) = (x: A) => m
//}


