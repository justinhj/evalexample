import scala.compiletime.{error}

object MacroPlay1 extends App:
  
  // inline guarantees a definition will be inlined at the point of use
  // this is forced, and at the typer phase so it can be used for type
  // level programming. TODO how
  // inline pattern matching (enabling type-level programming), macros 
  // (enabling compile-time, generative, metaprogramming) and runtime code
  // generation (multi-stage programming).
  
  inline val finalInt = 10
  //inline val finalList = (1,2,3)
  
  // failure with the error method...
  
  inline def evenExpansion(n: Int): Int = {
    if(n%2 == 0) error("Cannot expand this with even numbers")
    else n + 1
  } 
  
  //println(s"hello ${evenExpansion(4)}") COMPILE ERROR
  println(s"hello ${evenExpansion(1)}")

  def f(a: Int): Int = a + 10
  
  Macros.assert(10 == f(0))
  //val what1 = '{f(10)}
  //val what = showExpr()

  val s1 = Macros.sum_m(Array(1,2,3,4))
  println(s1)

//  import org.justinhj.typeclasses.applicative.{given,_}
//  
//  def map3[F[_] : Applicative,A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
//    val app = Applicative[F]
//    val ff = app.pure((a:A) => (b:B) => (c:C) => f(a,b,c))
//    fc.ap(fb.ap(fa.ap(ff)))
//  } 
//
//  // Using map3 
//  val s2 = map3(Option(10), Option(20), Option(30))((a,b,c) => a + b + c)
//  println(s2)










