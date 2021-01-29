package livevideos

object Video5Backup extends App {

  // Functors and error handling
  trait Functor[F[_]]:
    extension[A,B](x: F[A])
      def map(f: A => B): F[B]
  end Functor

  given Functor[List] with {
    extension[A,B](x: List[A])
      def map(f: A => B): List[B] = {
        x match {
          case hd :: tl => f(hd) :: tl.map(f)
          case Nil => Nil
        }
      }
  }

  val l1 = List(1,2,3)

  def f(a: Int): Int = a + 1
  def g(a: Int): Int = a * 1

  val lm1 = l1.map(f)

  println(s"mapped l1 $lm1")

  def h(a: Int) = a

  val lm2 = l1.map(h) == l1
  println(s"lm2 should be true: $lm2")

  val lm3 = l1.map(a => f(g(a))) == l1.map(f).map(g)
  println(s"lm3 should be true: $lm3")

  // First develop StringEither then we can do the generic one afterwards

  // type StringEither[A] = Either[String,A]

  // given Functor[StringEither] with {
  //   extension[A,B](x: StringEither[A])
  //     def map(f: A => B): StringEither[B] = {
  //       x match {
  //         case Right(a) => Right(f(a))
  //         case Left(err) => Left(err)
  //       }
  //     }
  // }

  // When talking about this need to explain the type lambda 

  given eitherFunctor[Err]: Functor[[X] =>> Either[Err,X]] with {
    extension[A,B](x: Either[Err,A]) def map(f: A => B) = x match {
      case Right(a) => Right(f(a))
      case Left(err) => Left(err)
    }
  }

  // We can now call a pure function using map on any type of either...

  val mapUpper = Right("y").map(_.toUpperCase)
  println(s"mapLookup $mapUpper")

  val mapUpperFail = Left("error").map(lookup)
  println(s"mapLookup fail $mapUpperFail")

  // Now we can introduce the idea of a Kliesli shaped function
  // and motivate the Monad

  def lookup(symbolName: String): Either[String,Int] = {
    val symbolTable = Map("Xabcd" -> 1, "Yabcd" -> 2)
    symbolTable.get(symbolName) match {
      case Some(value) => Right(value)
      case None => Left(s"Symbol not found $symbolName")
    }
  }

  val lookupS1 = lookup("x")
  println(s"lookupS1 is $lookupS1")

  val lookupS2 = lookup("z")
  println(s"lookupS2 is $lookupS2")
  
  trait Monad[F[_]] extends Functor[F]:

     /** The unit value for a monad */
     def pure[A](x: A): F[A]

     extension [A, B](x: F[A])
        /** The fundamental composition operation */
        def flatMap(f: A => F[B]): F[B]

        /** The `map` operation can now be defined in terms of `flatMap` */
        def map(f: A => B) = x.flatMap(f.andThen(pure))

  end Monad

  // Implementation of Monad for Either
  given eitherMonad[Err]: Monad[[X] =>> Either[Err,X]] with {
    def pure[A](a: A): Either[Err, A] = Right(a)
    extension [A,B](x: Either[Err,A]) def flatMap(f: A => Either[Err, B]) = {
      x match {
        case Right(a) => f(a)
        case Left(err) => Left(err)
      }
    }

  }

  // Call pure... 
  val symbol1 = summon[Monad[[X] =>> Either[String,X]]].pure("Symbol1")
  println(s"symbol1 is $symbol1")

  // Call flatMap - error case
  val lookupS3 = symbol1.flatMap(lookup)
  println(s"lookupS3 is $lookupS3")

  // Call flatMap - success case
  val symbol2 = summon[Monad[[X] =>> Either[String,X]]].pure("x")
  val lookupS4 = symbol2.flatMap(lookup)
  println(s"lookupS4 is $lookupS4")

  // Sequencing two functions
  def validateSymbol(symbolName: String): Either[String, String] = {
    if(symbolName.size == 0) Left("Symbol is empty")
    else {
      if(symbolName(0).isUpper == false) Left("Symbol must start with an uppercase letter")
      else {
        val rest = symbolName.substring(1).toList
        if(rest.forall(_.isLower)) Right(symbolName)
        else Left("Trailing characters must be lowercase")
      }
    }
  }

  // Validate then lookup
  val symbol3 = summon[Monad[[X] =>> Either[String,X]]].pure("Xabcd")

  val what1 = symbol3.flatMap(s => validateSymbol(s)).flatMap(lookup)
  println(s"what1 is $what1")

  // Same again with an invalid symbol
  val symbol4 = summon[Monad[[X] =>> Either[String,X]]].pure("abcd")

  val what2 = symbol4.flatMap(s => validateSymbol(s)).flatMap(lookup)
  println(s"what2 is $what2")

  // Nicer syntax for pure

  object Monad:
    def apply[F[_]](using m: Monad[F]) = m

  val m = Monad[[X] =>> Either[String, X]]  

  val symbol5 = Monad[[X] =>> Either[String,X]].pure("Yabcd")

  // type MyEither[A] = [E] =>> Either[E,A]

  //  val symbol6: Either[String, String] = Monad[EEither].pure("Yabcd")
}
