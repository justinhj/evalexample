package org.justinhj.typeclasses.monad

import org.justinhj.typeclasses.applicative.Applicative
import org.justinhj.typeclasses.functor.Functor

object Monad:
  def apply[F[_]](using m: Monad[F]) = m

trait Monad[F[_]] extends Applicative[F]:

 // The unit value for a monad
 def pure[A](x:A):F[A]
 
 extension[A,B](fa :F[A])
   // The fundamental composition operation
   def fflatMap(f :A=>F[B]):F[B]
   
   // Monad can also implement `ap` in terms of `map` and `flatMap`
   def ap(fab: F[A => B]): F[B] = {
     fab.fflatMap {
       f =>
         fa.fflatMap {
           a => 
             pure(f(a))
         }
     }
     
   }

end Monad

given eitherMonad[Err]: Monad[[X] =>> Either[Err,X]] with
  def pure[A](a: A): Either[Err, A] = Right(a)
  extension [A,B](x: Either[Err,A]) def fflatMap(f: A => Either[Err, B]) = {
    x match {
      case Right(a) => f(a)
      case Left(err) => Left(err)
    }
  }

given listMonad: Monad[List] with
  def pure[A](a: A): List[A] = List(a)

  extension[A,B](x: List[A])
    def fflatMap(f: A => List[B]): List[B] = {
      x match {
        case hd :: tl => f(hd) ++ tl.fflatMap(f)
        case Nil => Nil
      }
    }

