package livevideos

object Video5Backup extends App {

  // Functors and error handling

  trait Functor[F[_]] {
    extension[A, B](x: F[A]) {
      def fmap(f: A => B): F[B]
    }
  }

  given Functor[List] with {
    extension[A, B](x: List[A]) {
      def fmap(f: A => B): List[B] = {
        x match {
          case hd :: tl => f(hd) :: tl.fmap(f)
          case tl => {
            Nil
          }
        }
      }
    }
  }

  given eitherFunctor[Err]: Functor[[X] =>> Either[Err,X]] with {
    extension[A,B](x: Either[Err,A]) def fmap(f: A => B) = x match {
      case Right(a) => {
        println("poop haha")
        Right(f(a))
      }
      case Left(err) => Left(err)
    }
  }

  val l1 = List(1,2,3,4,5)

  println(l1.fmap(a => a + 1))

  // Notes from https://wiki.haskell.org/Functor
  // Identity law
  // When performing the mapping operation, if the values in the functor are mapped to themselves,
  // the result will be an unmodified functor.

  l1.fmap(identity) == l1

  // Composition law
  // If two sequential mapping operations are performed one after the other using two functions, the
  // result should be the same as a single mapping operation with one function that is equivalent to
  // applying the first function to the result of the second.

  def f(a: Int) = a + 1
  def g(a: Int) = a * -1

  assert(l1.fmap(g.compose(f)) == l1.fmap(f).fmap(g))
  println(l1.fmap(g.compose(f)))
  println(l1.fmap(f).fmap(g))

  // These two laws ensure that functors behave the way they were intended. The values of the functor are
  // only modified by the function provided to the mapping operation. The mapping operation by itself
  // does not modify the values in the functor, only the function. The structure of the functor remains
  // unchanged and only the values are modified. fmap returns an identical functor as the original, with
  // its values swapped to the result of calling a given function with the original value as an argument.

  type StringEither[A] = Either[String,A]

  given Functor[StringEither] with {
    extension[A,B](x: StringEither[A])
      def fmap(f: A => B): StringEither[B] = {
        x match {
          case Right(a) => Right(f(a))
          case Left(err) => Left(err)
        }
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


}
