object WriterTest extends App {
  
  object Functor:
    def apply[F[_]](using f: Functor[F]) = f

  trait Functor[F[_]]:
    extension [A, B](x: F[A])
      def map(f: A => B): F[B]
  
  object Applicative:
    def apply[F[_]](using a: Applicative[F]) = a

  trait Applicative[F[_]] extends Functor[F]:
    def pure[A](x:A):F[A]

    extension [A,B](x: F[A])
      def ap(f: F[A => B]): F[B]
  
      def map(f: A => B): F[B] = {
        x.ap(pure(f))
      }

    extension [A,B,C](fa: F[A]) def map2(fb: F[B])(f: (A,B) => C): F[C] = {
      val fab: F[B => C] = fa.map((a: A) => (b: B) => f(a,b))
      fb.ap(fab)
    }

  end Applicative


  object Monad:
    def apply[F[_]](using m: Monad[F]) = m

  trait Monad[F[_]] extends Applicative[F]:

    // The unit value for a monad
    def pure[A](x:A):F[A]

    extension[A,B](fa :F[A])
      // The fundamental composition operation
        def flatMap(f :A=>F[B]):F[B]
  
        // Monad can also implement `ap` in terms of `map` and `flatMap`
        def ap(fab: F[A => B]): F[B] = {
          fab.flatMap {
            f =>
              fa.flatMap {
                a =>
                  pure(f(a))
              }
          }
  
        }

  end Monad

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

  given listMonad: Monad[List] with
    def pure[A](a: A): List[A] = List(a)
  
    extension[A,B](x: List[A])
      def flatMap(f: A => List[B]): List[B] = {
        x match {
          case hd :: tl => f(hd) ++ tl.flatMap(f)
          case Nil => Nil
        }
      }

  case class Transformer[F[_]: Monad,A](val wrapped: F[A])
  
  given transformerMonad[F[_]: Monad]: Monad[[X] =>> Transformer[F,X]] with {

    def pure[A](a: A): Transformer[F,A] = Transformer(summon[Monad[F]].pure(a))

    extension [A,B](fa: Transformer[F,A]) 
      def flatMap(f: A => Transformer[F,B]) = {
        val ffa: F[B] = Monad[F].flatMap(fa.wrapped) {
          case a => {
            f(a).wrapped.map {
              case b =>
                b
            }
          }
        }
        Transformer(ffa)
      }
  }
  
  type EString[A] = Either[String,A]

  def incrementEven(a: Int): Transformer[EString,Int] = {
    if(a % 2 == 1) Transformer(Left("Odd number provided"))
    else Transformer(Right(a + 1))
  }

  def doubleOdd(a: Int): Transformer[EString, Int] = {
    if(a % 2 == 0) Transformer(Left("Even number provided"))
    else Transformer(Right(a * 2))
  }
  
  val writerExample = incrementEven(8)
  
  val m = Monad[[A] =>> Transformer[EString, A]]

  val p1: Transformer[EString,Int]  = Monad[[A] =>> Transformer[EString, A]].pure(10)
  
  //Monad[[A] =>> Transformer[EString, A]].flatmap(p1)(doubleOdd)

  //WriterTest.transformerMonad[Monad[[A] =>> Transformer[EString, A]]]

  val a1: Transformer[EString,Int] = WriterTest.transformerMonad[EString].pure(10)
  println(a1)

  val a2 = WriterTest.transformerMonad[EString].flatMap(a1)(incrementEven)
  println(a2)

//  import WriterTest.{given,_}
//  
//  println(a2.flatMap(doubleOdd))
  
  
  //  val example = writerExample.flatMap(doubleOdd) // Error ambiguous F
//  
//  println(example)

  /*
  /Users/justinhj/evalexample/src/main/scala/WriterTest.scala:123:31
value flatMap is not a member of WriterTest.Transformer[WriterTest.EString, Int].
An extension method was tried, but could not be fully constructed:

    WriterTest.transformerMonad[F](
      /* ambiguous: both method eitherMonad in object WriterTest and object optionMonad in object WriterTest match type WriterTest.Monad[F] */
        summon[WriterTest.Monad[F]]
    ).flatMap()
   */
}
