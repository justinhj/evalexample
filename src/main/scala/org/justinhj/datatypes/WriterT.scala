package org.justinhj.datatypes

import org.justinhj.typeclasses.monad.Monad
import org.justinhj.typeclasses.functor.Functor
import org.justinhj.typeclasses.applicative.Applicative
import org.justinhj.typeclasses.monoid.{given,_}

object WriterT:
  // lift takes any monadic effect and transforms to a WriterT around that monad
  def lift[F[_],W, A](fa: F[A])(using m: Monoid[W], F: Functor[F]): WriterT[F,W,A] =
    WriterT(F.map(fa)(a => (m.zero, a)))

case class WriterT[F[_],W,A](private val wrapped: F[(W,A)]):
  // tell let's us write to the log without affecting the current computed value
  def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
    WriterT(wrapped.map{
      (l2,a) =>
        (m.combine(l2, l1), a)
    })

  // tell let's us write to the log without affecting the current computed value
  // and we can access the value when making the log
  def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
    WriterT(wrapped.map{
      (l2,a) =>
        (m.combine(l2, faw(a)), a)
    })
    
  // written is so you can grab the log
  def written(using f: Functor[F]): F[W] =
    f.map(wrapped)(_._1)

  // value is so you can grab the value and drop the log
  def value(using f: Functor[F]): F[A] =
    f.map(wrapped)(_._2)

  // unwrap
  def unwrap() = wrapped