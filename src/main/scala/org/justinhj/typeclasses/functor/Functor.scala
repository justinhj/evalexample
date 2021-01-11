package org.justinhj.typeclasses.functor

trait Functor[F[_]]:
  extension [A, B](x: F[A])
    def fmap(f: A => B): F[B]

given Functor[List] with {
  extension[A,B](x: List[A])
    def fmap(f: A => B): List[B] = {
      x match {
        case hd :: tl => f(hd) :: tl.map(f)
        case Nil => Nil
      }
    }
}

given eitherFunctor[Err]: Functor[[X] =>> Either[Err,X]] with {
  extension[A,B](x: Either[Err,A]) def fmap(f: A => B) = x match {
    case Right(a) => Right(f(a))
    case Left(err) => Left(err)
  }
}