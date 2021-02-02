object Video7 extends App:
  
  trait Monad1[F[_]]:
    def unit[A](a:A): F[A]
    def compose[A,B,C](lf: A => F[B], rf: B => F[C]): A => F[C]

  object Monad1:
    def apply[F[_]](using m: Monad1[F]) = m

  given optionMonad1: Monad1[Option] with
    def unit[A](a:A) = Option(a)
    def compose[A,B,C](lf: A => Option[B], rf: B => Option[C]): A => Option[C] = {
      a => 
        lf(a) match {
          case Some(b) =>
            rf(b) match {
              case Some(c) =>
                Option(c)
              case None => None
            }
          case None => None          
        } 
    }

  // Sample usage
  def f(n:Int): Option[Int] = if n == 4 then None else Option(n)
  def g(n:Int): Option[Boolean] = if n%2==1 then Option(true) else Option(false)
  def h(b:Boolean): Option[String] = if b then Some("Winner!") else None

  val fcomposed = Monad1[Option].compose(f,g)
  val fghComposed = Monad1[Option].compose(fcomposed, h)
  
  println(fghComposed(1))
  println(fghComposed(2))
  println(fghComposed(3))
  println(fghComposed(4))

  // Monad laws
  val m1 = Monad1[Option]
  
  // left and right identity
  println(
    m1.compose(f, m1.unit)(1) == f(1))
  println(
    f(1) == m1.compose(f, m1.unit)(1))
  
  // associative law
  println(
    m1.compose(m1.compose(f,g), h)(1) == m1.compose(f, m1.compose(g,h))(1)
  )
    
  // Flatmap
  def flatMap[F[_],A,B](fa:F[A])(f: A => F[B])(using m: Monad1[F]): F[B] = {
    // F[A] => F[A]
    // A => F[B]
    m.compose((a: F[A]) => identity(a), a => f(a))(fa)
  }

  println(flatMap(f(1))(g))

  // How about compose with flatmap?
  {
    import org.justinhj.typeclasses.monad.{given,_}

    def compose[F[_],A,B,C](lf: A => F[B], rf: B => F[C])(using m: Monad[F]): A => F[C] = {
      a =>
        val fb = m.pure(a).flatMap(lf)
        fb.flatMap(rf) 
    }

    val c2 = compose(f,g)(1)
    println(s"compose $c2")
  }

