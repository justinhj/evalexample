package org.justinhj.typeclasses

trait Functor[F[_]]:
  extension [A, B](x: F[A])
    def fmap(f: A => B): F[B]
