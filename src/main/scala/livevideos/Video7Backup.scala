object Video7Backup extends App:

  // 1. Monads in Category theory 
  // Diagram?
  // Monads are just flatMap and map and unit
  // But why?
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
  
  def validateSymbol(symbol: String): Option[String] = 
    val regex = "[A-Z]+[a-z]*".r
    if regex.matches(symbol) then Some(symbol) else None

  def lookupSymbol(symbol: String): Option[Int] =
    val symbolTable: Map[String, Int] = Map("Alpha" -> 10, "Beta" -> 20)
    symbolTable.get(symbol)

  val x = optionMonad1.compose(validateSymbol, lookupSymbol)

  println(x("a"))
  println(x("alpha"))
  println(x("Alpha"))
  println(x("Beta"))
  println(x("Betamax"))

  // 5. Monad laws
  // Remember with the category of Scala functions we had to have an identity morphism and function morphisms
  // are associative? Monad is the category of Kliesli functions (or arrows) so it needs the same two things

  val m1 = Monad1[Option]

  // left identity
  println(
    m1.compose(validateSymbol, m1.unit)("alpha") == validateSymbol("alpha"))
  println(
    m1.compose(validateSymbol, m1.unit)("Alpha") == validateSymbol("Alpha"))

  // right identity
  println(
    validateSymbol("alpha") == m1.compose(validateSymbol, m1.unit)("alpha"))
  println(
    validateSymbol("Alpha") == m1.compose(validateSymbol, m1.unit)("Alpha"))

  // associativity

  def double(n: Int): Option[Int] =
    Some(n + n)
 
  println(
    m1.compose(
      m1.compose(validateSymbol, lookupSymbol), double)("Beta"))

  println(
    m1.compose(
      validateSymbol, m1.compose(lookupSymbol, double))("Beta"))

  // 6. flatMap with compose?
  
  def flatMap[F[_],A,B](fa: F[A])(f: A => F[B])(using m: Monad1[F]): F[B] = {
    m.compose((fa: F[A]) => fa, a => f(a))(fa)
  }

  println(
    flatMap(validateSymbol("Beta"))(lookupSymbol))

  // 7. Conclusion since flatmap can be implemented with compose you can see that unit and flatMap is indeed a Monad

