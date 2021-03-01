object Minimized extends App {
  
  trait Monad[F[_]]:
    def pure[A](a: A): F[A]
    
    extension[A,B](fa :F[A]) 
      def map(f: A => B): F[B] = fa.flatMap(a => pure(f(a)))
      def flatMap(f :A=>F[B]):F[B]
  end Monad

  // Instances 
//  given eitherMonad[Err]: Monad[[X] =>> Either[Err,X]] with
//    def pure[A](a: A): Either[Err, A] = Right(a)
//    extension [A,B](x: Either[Err,A]) def flatMap(f: A => Either[Err, B]) = {
//      x match {
//        case Right(a) => f(a)
//        case Left(err) => Left(err)
//      }
//    }

  given optionMonad: Monad[Option] with
    def pure[A](a: A) = Some(a)
    extension[A,B](fa: Option[A])
      def flatMap(f: A => Option[B]) = {
        fa match {
          case Some(a) =>
            f(a)
          case None =>
            None
        }
      }

  case class Transformer[F[_]: Monad,A](val wrapped: F[A])

  given transformerMonad[F[_]: Monad]: Monad[[X] =>> Transformer[F,X]] with {

    def pure[A](a: A): Transformer[F,A] = Transformer(summon[Monad[F]].pure(a))

    extension [A,B](fa: Transformer[F,A])
      def flatMap(f: A => Transformer[F,B]) = {
        val ffa: F[B] = summon[Monad[F]].flatMap(fa.wrapped)(a => f(a).wrapped)
        Transformer(ffa)
      }
  }
  
  type TransformerOption[A] = Transformer[Option, A]
  
  val pure10 = summon[Monad[TransformerOption]].pure(10)
  val fm = pure10.flatMap(a => Transformer(Option(a + 1)))
  println(fm)
}
