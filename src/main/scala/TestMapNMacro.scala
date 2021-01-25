object TestMapNMacro extends App {

  //import MapNMacro._

 // val tm = sum_m(1,2,3,4,5)

  //println(s"Sum of array $tm")

  import org.justinhj.typeclasses.applicative.{given,_}

  def map3[F[_] : Applicative,A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    val app = Applicative[F]
    val ff = app.pure((a:A) => (b:B) => (c:C) => f(a,b,c))
    fc.ap(fb.ap(fa.ap(ff)))
  }

  def productn[F[_] : Applicative,A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
    val app = summon[Applicative[F]]
    val ff = fa.fmap( 
      a => 
        (b:B) => (a,b)
    )
    fb.ap(ff)
  }

  def productn[F[_] : Applicative,A,B,C](fa: F[A], fb: F[B], fc: F[C]): F[(A,B,C)] = {
    val app = summon[Applicative[F]]
    val ff = app.pure((a:A) => (b:B) => (c:C) => (a,b,c))
    fc.ap(fb.ap(fa.ap(ff)))
  }
  
  // Using map3 
  val s2 = map3(Option(10), Option(20), Option(30))((a,b,c) => a + b + c)
  println(s2)
  
  // Product 2
  println(s"p2 ${productn(Option(10), Option("Nero"))}")
  println(s"p2 ${productn(Option(10), Option("Nero"), Option(2.0))}")

  // Mapn test
  // println(s"productM test ${MapNMacro.productM(5, 10, 20, 20, 30.0)}") // Double require int
  println(s"productM test ${MapNMacro.productM(500, 9220981, 1982282, 10, 20, 20, 30)}")
  println(s"productM test ${MapNMacro.productM()}")
  println(s"productM test ${MapNMacro.productM(1,2,3)}")


}
