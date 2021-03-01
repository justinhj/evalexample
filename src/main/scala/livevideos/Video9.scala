package livevideos

import org.justinhj.typeclasses.monad.{given,_}
import org.justinhj.typeclasses.monoid.{given,_}
import org.justinhj.typeclasses.functor.{given,_}

object Video9 extends App:

  case class WriterT[F[_],W,A](wrapped: F[(W,A)]):
    def tell(l1: W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
      WriterT(wrapped.map{(l2,a) => (m.combine(l2, l1), a)})

    def tellWith(faw: A => W)(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
      WriterT(wrapped.map{(l2,a) => (m.combine(l2, faw(a)), a)})
        
  object WriterT:
    def lift[F[_],W,A](fa: F[A])(using m: Monoid[W], f: Functor[F]): WriterT[F,W,A] =
      WriterT(f.map(fa)(a => (m.zero, a)))
  
  implicit final class WriterTOps[F[_]: Monad, W: Monoid, A](private val fa: WriterT[F,W,A]) {
    def flatMap[B](f: A => WriterT[F,W,B]): WriterT[F,W,B] =
      Monad[[A] =>> WriterT[F,W,A]].flatMap(fa)(a => f(a))
  }
  
  given writerTransformerMonad[F[_]: Monad, W: Monoid]: Monad[[X] =>> WriterT[F,W,X]] with
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
  
  type StringEither[A] = Either[String,A]
  type StringEitherWriter[A] = WriterT[StringEither,List[String],A]
  
  def incrementEven(n: Int): StringEither[Int] =
    if n % 2 == 0 then Right(n+1) else Left("Not an even number")
    
  def doubleOdd(n: Int): StringEither[Int] =
    if n % 2 == 1 then Right(n *2) else Left("Not an odd number")
    
  val program1: StringEitherWriter[Int] = for (
    a <- Monad[StringEitherWriter].pure(10).tellWith(a => List(s"Initialized with $a"));
    b <- WriterT.lift[StringEither,List[String],Int](incrementEven(a)).tellWith(a => List(s"incremented to $a"));
    c <- WriterT.lift[StringEither,List[String],Int](doubleOdd(b)).tellWith(a => List(s"doubled to $a"))
  ) yield c
  
  println(s"program1 $program1")











