object Minimized extends App {
  
  trait Monad[F[_]]:
    def pure[A](a: A): F[A]
    
    extension[A,B](fa :F[A]) 
      def map(f: A => B): F[B] = fa.flatMap(a => pure(f(a)))
      def flatMap(f :A=>F[B]):F[B]
  end Monad

  // Instances 
  given eitherMonad[Err]: Monad[[X] =>> Either[Err,X]] with
    def pure[A](a: A): Either[Err, A] = Right(a)
    extension [A,B](x: Either[Err,A]) def flatMap(f: A => Either[Err, B]) = {
      x match {
        case Right(a) => f(a)
        case Left(err) => Left(err)
      }
    }

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

  given readerMonad[Ctx]: Monad[[X] =>> Ctx => X] with

    def pure[A](x: A): Ctx => A =
      ctx => x
  
    extension [A,B](x: Ctx => A)
      def flatMap(f: A => Ctx => B): Ctx => B =
        ctx => f(x(ctx))(ctx)

  end readerMonad

  val p1 = summon[Monad[[A1] =>> Int => A1]].pure(15)
  val p2 = p1.flatMap {
    a =>
      s =>
        println(s"a $a + s $s = ${a + s}")
        a + s
  }
  println(p2(10))
  
  // This extension method is saying if you have an F[_] and an A
  // and an instance of type fa, then that has an extension method flatMap
  
  extension[F[_], A](fa: F[A])(using monad: Monad[F])
    def flatMap[B](f: A => F[B]) = monad.flatMap(fa)(f)

  // But Monad has the extension method
  //    extension[A,B](fa :F[A])
  //      def map(f: A => B): F[B] = fa.flatMap(a => pure(f(a)))
  //      def flatMap(f :A=>F[B]):F[B]

  type TransformerOption[A] = Transformer[Option, A]
  
  val pure10 = summon[Monad[[A1] =>> Transformer[Option, A1]]].pure(10)
  
  // value flatMap is not a member of Minimized.Transformer[Option, Int]
  
  val m = summon[Monad[[A1] =>> Transformer[Option, A1]]]
  
  m.flatMap(pure10)(a => Transformer[Option,Int](Option(a + 1)))
  
  val a1: Transformer[Option,Int] = pure10.flatMap(a => Transformer[Option,Int](Option(a + 1)))
  
//  val fm = pure10.flatMap(a => Transformer[Option,Int](Option(a + 1)))
//  println(fm)
  
//  val prog1 = for (
//    a <- summon[Monad[[A1] =>> Transformer[Option, A1]]].pure(10);
//    b <- Transformer[Option,Int](Option(a + 1));
//    c <- Transformer[Option,Int](Option(b * 2))
//  ) yield c
//  
//  println(prog1)
}
