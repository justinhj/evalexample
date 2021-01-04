package org.justinhj.typeclasses

trait Applicative[F[_]] extends Functor[F]:

  /** The unit value for a monad */
  def pure[A](x:A):F[A]

  // TODO ap and map2 etc

end Applicative