package org.justinhj.datatypes

import org.justinhj.typeclasses.monad.Monad
import org.justinhj.typeclasses.functor.Functor
import org.justinhj.typeclasses.applicative.Applicative
import org.justinhj.typeclasses.monoid.{given,_}

object StateT:
  // lift takes any monadic effect and transforms to a StateT around that monad
  def lift[F[_],S, A](fa: F[A], s: S)(using F: Applicative[F]): StateT[F,S,A] =
    StateT(F.map(fa)(a => (s,a)))

case class StateT[F[_],S,A](val wrapped: F[(S,A)])