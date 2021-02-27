package livevideos

import org.justinhj.typeclasses.monad.{given,_}
import org.justinhj.typeclasses.monoid.{given,_}
import org.justinhj.typeclasses.functor.{given,_}

object Video9 extends App:

  case class WriterT[F[_],W,A](wrapped: F[(W,A)])  

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
  
  def incrementEven(n: Int): Either[String, Int] =
    if n % 2 == 0 then Right(n+1) else Left("Not an event number")
  
  def doubleOdd(n: Int): Either[String, Int] =
    if n % 2 == 1 then Right(n * 2) else Left("Not an odd number")

  writerTransformerMonad2[[A1] =>> Either[String, A1],List[String]].
    flatMap(we1)(a => WriterT.lift(incrementEven(a)))

  val p1 = Monad[[A2] =>> WriterT[[A1] =>> Either[String, A1],List[String], A2]].pure(10)
  Monad[[A2] =>> WriterT[[A1] =>> Either[String, A1],List[String], A2]].flatMap(p1)(a => WriterT.lift(incrementEven(a)))
  
  def prog1(n: Int): WriterT[[A1] =>> Either[String, A1], List[String], Int] = for (
    a <- WriterT.lift(incrementEven(n));
    b <- WriterT.lift(doubleOdd(a))
  ) yield b

  println(prog1(8))
  
  
  println("Test")

