object WriterTest extends App {

  trait Semigroup[A]:
    def combine(al: A, ar: A): A

  object Semigroup:
    def apply[A](using s: Semigroup[A]) = s

  trait Monoid[A] extends Semigroup[A]:
    def zero: A

  object Monoid:
    def apply[A](using m: Monoid[A]) = m

  given stringMonoid: Monoid[String] with
    def zero = ""
    def combine(al:String, ar:String) = al + ar

  given listMonoid[A]: Monoid[List[A]] with
    def zero = List.empty[A]
    def combine(al:List[A], ar:List[A]) = al ++ ar

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

  case class WriterT[F[_]: Monad,W,A](val wrapped: F[(W,A)])
  
  given writerTMonad[F[_]: Monad,W: Monoid]: Monad[[X] =>> WriterT[F,W,X]] with {

    def pure[A](a: A): WriterT[F,W,A] = WriterT(summon[Monad[F]].pure((Monoid[W].zero,a)))

    extension [A,B](fa: WriterT[F,W,A]) 
      def flatMap(f: A => WriterT[F,W,B]) = {
        val ffa: F[(W,B)] = Monad[F].flatMap(fa.wrapped) {
          case (wa,a) => {
            f(a).wrapped.map {
              case (wb, b) =>
                (Monoid[W].combine(wa,wb), b)
            }
          }
        }
        WriterT(ffa)
      }
  }


  type EString[A] = Either[String,A]

  def incrementEven(a: Int): WriterT[EString,List[String],Int] = {
    if(a % 2 == 1) WriterT(Left("Odd number provided"))
    else WriterT(Right((List("Inc even"), a + 1)))
  }

  def doubleOdd(a: Int): WriterT[EString, List[String], Int] = {
    if(a % 2 == 0) WriterT(Left("Even number provided"))
    else WriterT(Right((List("Double odd"), a + a)))
  }
  
  val writerExample = incrementEven(8)
  val example = writerExample.flatMap(doubleOdd) // Error ambiguous F
//  
//  println(example)

}