package org.justinhj.typeclasses

object Monad:
  def apply[F[_]](using m: Monad[F]) = m

trait Monad[F[_]] extends Functor[F]:

 // The unit value for a monad
 def pure[A](x:A):F[A]
 
 extension[A,B](x:F[A])
   // The fundamental composition operation
   def fflatMap(f:A=>F[B]):F[B]
   
   // The `map` operation can now be defined in terms of `flatMap`
   def fmap(f:A=>B)=x.fflatMap(f.andThen(pure))

end Monad

given eitherMonad[Err]: Monad[[X] =>> Either[Err,X]] with {
  def pure[A](a: A): Either[Err, A] = Right(a)
  extension [A,B](x: Either[Err,A]) def fflatMap(f: A => Either[Err, B]) = {
    x match {
      case Right(a) => f(a)
      case Left(err) => Left(err)
    }
  }
}