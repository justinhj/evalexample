object Video7Prep extends App:

  // Introduction slides and video camera. Monads in Category theory 
  // Diagram
  // Monads are just flatMap and map and unit
  // But why
  // Functions map to category theory so we have identity and associative functions
  // Monads are also a category. Where kleisli arrows are functions 
  // identity is unit and compose is associativity

  // 2. Compose and Unit are identity and associativity respectively 
  
  trait Monad1[F[_]]:
    def unit[A](a: A): F[A]
    def compose[A,B,C](lf: A => F[B], rf: B => F[C]): A => F[C]

  object Monad1:
    def apply[F[_]](using m: Monad1[F]) = m
  
  // 3. Instance for Option
  
  given optionMonad1: Monad1[Option] with
    def unit[A](a: A) = Option(a)

    def compose[A,B,C](f: A => Option[B], g: B => Option[C]): A => Option[C] =  {
      a => 
        f(a) match {
          case Some(b) =>
            g(b) match {
              case Some(c) =>
                Option(c)
              case None =>
                None
            }
          case None =>
            None
        }
    }

  // 4. Sample usage
  def f(n: Int): Option[Int] = if n == 4 then None else Option(n)
  def g(n: Int): Option[Boolean] = if n % 2 == 1 then Option(true) else Option(false)
  def h(b: Boolean): Option[String] = if b then Some("Winner!") else None
  
  val fcomposed = Monad1[Option].compose(f, g)

  println(fcomposed(1))
  println(fcomposed(2))
  println(fcomposed(3))
  println(fcomposed(4))

  // 5. Monad laws
  // Remember with the category of Scala functions we had to have an identity morphism and function morphisms
  // are associative? Monad is the category of Kliesli functions (or arrows) so it needs the same two things

  val m1 = Monad1[Option]

  // left identity
  println(
    m1.compose(f, m1.unit)(1) == f(1))

  // right identity
  println(
    f(1) == m1.compose(f, m1.unit)(1))

  // associativity
 
  println(
    m1.compose(
      m1.compose(f, g), h)(1))

  println(
    m1.compose(
      f, m1.compose(g, h))(1))

  // 6. flatMap with compose?
  
  def flatMap[F[_],A,B](fa: F[A])(f: A => F[B])(using m: Monad1[F]): F[B] = {
    m.compose((fa: F[A]) => fa, a => f(a))(fa)
  }

  println(
    flatMap(f(1))(g))

  // 7. Conclusion since flatmap can be implemented with compose you can see that unit and flatMap is indeed a Monad

