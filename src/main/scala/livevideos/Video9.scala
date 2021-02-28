package livevideos

import org.justinhj.typeclasses.monad.{given,_}
import org.justinhj.typeclasses.monoid.{given,_}
import org.justinhj.typeclasses.functor.{given,_}

object Video9 extends App:

  case class WriterT[F[_],W,A](wrapped: F[(W,A)]):
    def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
      WriterT(wrapped.map{
        (l2,a) =>
          (m.combine(l2, l1), a)
      })
    def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
      WriterT(wrapped.map{
        (l2,a) =>
          (m.combine(l2, faw(a)), a)
      })
        
  object WriterT:
    def lift[F[_],W,A](fa: F[A])(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
      WriterT(f.map(fa)(a => (m.zero, a)))

  given writerTransformerMonad2[F[_]: Monad, W: Monoid]: Monad[[X] =>> WriterT[F,W,X]] with
    def pure[A](a: A): WriterT[F,W,A] = WriterT.lift(Monad[F].pure(a))
    
    extension [A,B](fa: WriterT[F,W,A])
      def flatMap(f: A => WriterT[F,W,B]): WriterT[F,W,B] = {
         val ffb: F[(W,B)] = Monad[F].flatMap(fa.wrapped){
           case (wa,a) =>
            f(a).wrapped.map {
              case (wb,b) =>
                (Monoid[W].combine(wa,wb), b)
            }
         }
         WriterT(ffb) 
      }
    
  
  val e1 : Either[String, Int] = Right(10)
  val we1 = WriterT.lift[[A1] =>> Either[String, A1],List[String], Int](e1)
  
  // TODO add the tell and tellWith commands 
  // Use them to do a sequence of code 

  implicit final class WriterTOps[F[_]: Monad, W: Monoid, A](private val fa: WriterT[F,W,A]) {
    def flatMap[B](f: A => WriterT[F,W,B]): WriterT[F,W,B] =
      Monad[[A] =>> WriterT[F,W,A]].flatMap(fa)(a => f(a))

    def map2[B,C](fb: WriterT[F,W,B])(f: (A,B) => C): WriterT[F,W,C] =
      Monad[[A] =>> WriterT[F,W,A]].map2(fa)(fb)(f)
  }

  // Some types to save us 'typing'
  type StringEither[A] = Either[String, A]
  type StringEitherWriter[A] = WriterT[StringEither, List[String],A]

  // Couple of example functions that use either for error handling
  def incrementEven(n: Int): StringEither[Int] =
    if n % 2 == 0 then Right(n+1) else Left("Not an event number")
  
  def doubleOdd(n: Int): StringEither[Int] =
    if n % 2 == 1 then Right(n * 2) else Left("Not an odd number")

  // insert here a program that uses the above without WriterT then transform to the following
  
  val prog1 = for (
    a <- Monad[StringEitherWriter].pure(10).
      tellWith(a => List(s"Initialized to $a"));
    b <- WriterT.lift[StringEither,List[String], Int](incrementEven(a)).
      tellWith(b1 => List(s"Incremented $a to $b1"));
    c <- WriterT.lift[StringEither,List[String], Int](doubleOdd(b))
  ) yield c
  
  println(prog1)
  
  println("Test")

