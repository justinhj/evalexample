package livevideos

object Video5 extends App {

  // Functors and error handling
  trait Functor[F[_]]:
    extension[A,B](x: F[A])
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

  val l1 = List(1,2,3)

  def f(a: Int): Int = a + 1
  def g(a: Int): Int = a * 1

  val lm1 = l1.fmap(f)

  println(s"mapped l1 $lm1")

  def h(a: Int) = a

  val lm2 = l1.fmap(h) == l1
  println(s"lm2 should be true: $lm2")

  val lm3 = l1.fmap(a => f(g(a))) == l1.fmap(f).fmap(g)
  println(s"lm3 should be true: $lm3")

  // type StringEither[A] = Either[String,A]

  // given Functor[StringEither] with {
  //   extension[A,B](x: StringEither[A])
  //     def fmap(f: A => B): StringEither[B] = {
  //       x match {
  //         case Right(a) => Right(f(a))
  //         case Left(err) => Left(err)
  //       }
  //     }
  // }

  given eitherFunctor[Err]: Functor[[X] =>> Either[Err,X]] with {
    extension[A,B](x: Either[Err,A]) def fmap(f: A => B) = x match {
      case Right(a) => Right(f(a))
      case Left(err) => Left(err)
    }
  }

  def lookup(symbolName: String): Either[String,Int] = {
    val symbolTable = Map("x" -> 1, "y" -> 2)
    symbolTable.get(symbolName) match {
      case Some(value) => Right(value)
      case None => Left(s"Symbol not found $symbolName")
    }
  }

  val lookupS1 = lookup("x")
  println(s"lookupS1 is $lookupS1")

  val lookupS2 = lookup("z")
  println(s"lookupS2 is $lookupS2")
  
  // We can now call a pure function using fmap on any type of either...

  val fmapUpper = Right("y").fmap(_.toUpperCase)
  println(s"fmapLookup $fmapUpper")

  val fmapUpperFail = Left("error").fmap(lookup)
  println(s"fmapLookup fail $fmapUpperFail")
}