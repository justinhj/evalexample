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
  
  def incrementEven(n: Int): Either[String, Int] =
    if n % 2 == 0 then Right(n+1) else Left("Not an event number")
  
  def doubleOdd(n: Int): Either[String, Int] =
    if n % 2 == 1 then Right(n * 2) else Left("Not an odd number")

  writerTransformerMonad2[[A1] =>> Either[String, A1],List[String]].
    flatMap(we1)(a => WriterT.lift(incrementEven(a)))

  type StringEither[A] = Either[String, A]

  type StringEitherWriter[A] = WriterT[StringEither, List[String],A]
  
  val m = summon[Monad[StringEitherWriter]]
  val p10 = m.pure(10).tellWith(a => List(s"Initialized to $a"))
  println(s"p10 $p10")
  
  val incremented = m.flatMap(p10)(a => WriterT.lift(incrementEven(a))).tellWith(a => List(s"Incremented to $a"))
  println(incremented)

  //Monad[StringEitherWriter].pure(10).tellWith(a => List(s"Initialized to $a")).flatMap(a => WriterT.lift(incrementEven(a)))
  
  println("Test")

