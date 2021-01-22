object Macros {
  import scala.quoted._

  // Assert is an inline, and it has an inlined expression as a param
  // We 
  inline def assert(inline expr: Boolean): Unit =
    ${ assertImpl('expr) }

  def assertImpl(expr: Expr[Boolean])(using Quotes) = '{
    if !$expr then
      throw AssertionError(s"failed assertion: ${${ showExpr(expr) }}")
    }

  def showExpr[T](expr: Expr[T])(using Quotes): Expr[String] =
    val code: String = expr.show
    Expr(code)


  def map[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])
            (using Type[T], Quotes): Expr[Unit] = '{
    var i: Int = 0
    while i < ($arr).length do
      val element: T = ($arr)(i)
      ${f('element)}
      i += 1
    }

  def sum(arr: Expr[Array[Int]])(using Quotes): Expr[Int] = '{
    var sum = 0
    ${ map(arr, x => '{sum += $x}) }
    sum
  }

  inline def sum_m(arr: Array[Int]): Int = ${sum('arr)}

  def debugSingleImpl(expr: Expr[Any])(using Quotes): Expr[Unit] =
    '{ println("Value of " + ${showExpr(expr)} + " is " + $expr) }
    
  // mapN looks like map3
  //   def map3[F[_] : Applicative,A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
  // so vararg list of n F[_] each with a different type
  // then you have the 

  import org.justinhj.typeclasses.applicative.{given,_}
  
  import scala.quoted.ToExpr.{given}
  
  // Simple macro that just displays something
  inline def play1(n: Int): Unit = 
      ${play1Expr('n)}
  
  def play1Expr(n: Expr[Int])(using Quotes): Expr[Unit] =  
  '{
        println(s"Value of n is ${$n}") 
    }
  
  def map2[F[_] : Applicative,A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
    val app = Applicative[F]
    val ff = app.pure((a:A) => (b:B) => f(a,b))
    fb.ap(fa.ap(ff))
  }
  
  def map3[F[_] : Applicative,A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    val app = Applicative[F]
    val ff = app.pure((a:A) => (b:B) => (c:C) => f(a,b,c))
    fc.ap(fb.ap(fa.ap(ff)))
  }

  def to[T: Type, R: Type](f: Expr[T] => Expr[R])(using Quotes): Expr[T => R] =
    '{ (x: T) => ${ f('x) } }

  def from[T: Type, R: Type](f: Expr[T => R])(using Quotes): Expr[T] => Expr[R] =
    (x: Expr[T]) => '{ $f($x) }

//  val f1: Expr[Int => String] =
//    to((x: Expr[Int]) => '{ $x.toString }) // '{ (x: Int) => x.toString }
//
//  val f2: Expr[Int] => Expr[String] =
//    from('{ (x: Int) => x.toString }) // (x: Expr[Int]) => '{ ((x: Int) => x.toString)($x) }
//  
//  f2('{2}) // '{ ((x: Int) => x.toString)(2) }

}
