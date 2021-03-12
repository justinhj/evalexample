object Minimized2 extends App {

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

  type TransformerOption[A] = Transformer[Option, A]

  val pure10 = summon[Monad[[A1] =>> Transformer[Option, A1]]].pure(10)


//  trait Extendable[A] {
//    def hello(): Extendable[A]
//  }
//  
//  given extendExtendable[A]: Extendable[A] with {
//    extension (a: A) def hello() = {
//      println("Hello")
//      this
//    }
//  }
//  
//  val e1 = "Justin".hello
  
  
  
  // an extension method on tuple
  
//  trait BinaryExtender[A,B]:
//    def identity(): BinaryExtender[A,B]
//  
//  given tupleBE[A,B]: BinaryExtender[A,B] with 
//    extension (tab: Tuple2[A,B]) def identity(): BinaryExtender[A,B] = this
//  

  case class Circle(x: Double, y: Double, radius: Double)

  extension (c: Circle)
    def double: Circle = c.copy(radius = c.radius * 2)
  
  val circle1 = Circle(0.0,0.0,10.0)
  val circle2 = circle1.double
  println(circle2)

  // chain the extension methods
  val circle3 = circle1.double.double
  println(circle3)

  // Circle that carries with it a value of type A
  case class CircleA[A](x: Double, y: Double, radius: Double, a: A)
  
  extension [A](c: CircleA[A])
    def double: CircleA[A] = c.copy(radius = c.radius * 2)


  val circleA1 = CircleA(0.0,0.0,10.0,"Hello")
  val circleA2 = circleA1.double
  println(circleA2)

  // chain the extension methods
  val circleA3 = circleA1.double.double
  println(circleA3)
  
  // Doesn't compile but should (?)
  //  val fm = pure10.flatMap(a => Transformer[Option,Int](Option(a + 1)))
  //  println(fm)
}
