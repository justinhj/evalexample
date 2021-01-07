package livevideos

object Video5 extends App {

 def f[A,B](a: A): B = ???
  
  // Total - every a returns b
  // Deterministic - every a returns the same every time
  // No side effects

  trait Functor[F[_]]:
    extension [A, B](x: F[A])
      def ffmap(f: A => B): F[B]
  
//  type StringEither[A] = Either[String, A]
//  
//  given Functor[StringEither] with
//    extension [A, B](x: StringEither[A])
//      def ffmap(f: A => B): StringEither[B] = {
//        x match {
//          case Right(a) => Right(f(a))
//          case Left(err) => Left(err)
//        }
//      }
  
  given eitherFunctor[E]: Functor[[A] =>> Either[E, A]] with
    extension [A, B](x: Either[E,A])
        def ffmap(f: A => B): Either[E,B] = {
          x match {
            case Right(a) => Right(f(a))
            case Left(err) => Left(err)
          }
        }
  
  
  val e1: Either[String, Int] = Right(10)
  val e2: Either[Int, Int] = Left(10)

  val e3 = e1.ffmap(a => a + 1)
  val e4 = e2.ffmap(a => a + 1)


  // Functors and error handling
  println(e3)
  println(e4)
}
