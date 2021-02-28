object WriterTOldSchool extends App  {

  // For reference this a Scala 2 type class way to do it 
  
  //import org.justinhj.datatypes.WriterT
  import org.justinhj.typeclasses.monad.{given,_}
  import org.justinhj.typeclasses.monoid.{given,_}

  // Implement WriterT using Cats implementation of Monad and Monoids

  case class WriterT[F[_]: Monad,W,A](val wrapped: F[(W,A)])

  implicit def writerTMonad[F[_]: Monad,W: Monoid]: Monad[[A] =>> WriterT[F,W,A]] = new Monad[[A] =>> WriterT[F,W,A]] {

    def pure[A](a: A): WriterT[F,W,A] = WriterT(Monad[F].pure((Monoid[W].zero,a)))

    extension [A,B](fa: WriterT[F,W,A])
      def flatMap(f: A => WriterT[F,W,B]): WriterT[F,W,B] = {
        val ffa: F[(W,B)] = Monad[F].flatMap(fa.wrapped) {
          case (wa,a) => {
            val what = f(a).wrapped
            Monad[F].map(what){
              case (wb, b) =>
                (Monoid[W].combine(wa,wb), b)
            }
          }
        }
        WriterT(ffa)
      }
  }

  // Use an implicit class conversion to add flatMap and map as methods to any WriterT ...

  implicit final class WriterTOps[F[_]: Monad, W: Monoid, A](private val fa: WriterT[F,W,A]) {
    def flatMap[B](f: A => WriterT[F,W,B]): WriterT[F,W,B] =
      Monad[[A] =>> WriterT[F,W,A]].flatMap(fa)(a => f(a))

    def map[B](f: A => B): WriterT[F,W,B] =
      Monad[[A] =>> WriterT[F,W,A]].map(fa)(a => f(a))
  }

  def incrementEven(a: Int): WriterT[[A] =>> Either[String, A],String,Int] = {
    if(a % 2 == 1) WriterT(Left[String, (String, Int)]("Odd number provided"))
    else WriterT(Right(("Inc even", a + 1)))
  }

  def doubleOdd(a: Int): WriterT[[A] =>> Either[String, A], String, Int] = {
    if(a % 2 == 0) WriterT(Left[String, (String, Int)]("Even number provided"))
    else WriterT(Right(("Double odd", a + a)))
  }

  // Step 1 can we flatMap?

  val writerExample = incrementEven(8).flatMap(doubleOdd)

  println(writerExample)

  // Step 2 can we use pure

  val p8 = Monad[[A1] =>> WriterT[[A] =>> Either[String, A], String, A1]].pure(8)

  println(p8.flatMap(incrementEven).flatMap(doubleOdd))

  // Step 3 use in a for comprehension?

  val r : WriterT[[A] =>> Either[String, A], String, Int] = for (
    a <- Monad[[A1] =>> WriterT[[A] =>> Either[String, A], String, A1]].pure(8);
    b <- incrementEven(a);
    c <- doubleOdd(b)
  ) yield c

  println(r)
}