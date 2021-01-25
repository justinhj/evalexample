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

  import org.justinhj.typeclasses.applicative.{given,_}

  def map3[F[_] : Applicative,A,B,C,D](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
    val app = Applicative[F]
    val ff = app.pure((a:A) => (b:B) => (c:C) => f(a,b,c))
    fc.ap(fb.ap(fa.ap(ff)))
  }

  //inline def d(n: Int): Unit = ${Macros.makeMapN()}

  //d

  // Using map3
  val s2 = map3(Option(10), Option(20), Option(30))((a,b,c) => a + b + c)
  println(s2)

//  // debug expression
//  val x = 1
//  inline def debugSingle(inline expr: Any): Unit = ${Macros.debugSingleImpl('expr)}
//
//  debugSingle(f(x) + 1)

  // Match types from Adam's thing

  // regular matching on a tuple... this just prints the head of the tuple

  def matchTuple(t: Tuple): Option[Any] = t match {
    case head *: tail => Some(head)
    case EmptyTuple => None
  }

  println(matchTuple((10, "x", true)))
  println(matchTuple(("y", 14)))
  println(matchTuple(Tuple()))

  def matchTupleLast(t: Tuple): Option[Any] = t match {
    case last *: EmptyTuple => Some(last)
    case EmptyTuple => None
    case head *: rest => matchTupleLast(rest)
  }

  println(matchTupleLast((10, "x", true)))
  println(matchTupleLast(("y", 14)))
  println(matchTupleLast(Tuple()))

  // Can match with types as well as values ... compute types dynamically

  type TupleMap[T <: Tuple, F[_]] <: Tuple = T match {
    case EmptyTuple => EmptyTuple
    case h *: t => F[h] *: TupleMap[t, F]
  }

  val tm1: Tuple2[Option[Int], Option[Boolean]] = Tuple2(Some(10), Some(true))
  val tm2: TupleMap[(Int, Boolean), Option] = Tuple2(Some(10), Some(false))

  // InverseMap takes a Tuple and removes the layer of type F[_] here Option
  // so Tuple2[Option[Int], Option[String]] is going to be the type Tuple2[Int,String]
  //

  def sequence[T <: Tuple](t: T): Option[Tuple.InverseMap[T, Option]] = {
    val unwrapped = t.productIterator.collect {case Some(s) => s}.toArray[Any]
    if(unwrapped.length == t.productArity)
      Some(Tuple.fromArray(unwrapped).asInstanceOf[Tuple.InverseMap[T, Option]])
    else
      None
  }

  println("sequence " + sequence(Tuple2(Option(10), Option("ten"))))
  println("sequence " + sequence(Tuple3(Option(12), None, Option("twelve"))))

  // Tuple has a bunch of type level things you can do like map, flatmap, inversemap
  // here's filter...

  type IsNotString[A] <: Boolean = A match {
    case String => false
    case _ => true
  }

  // Here I enforce a type that is any tuple unless it contains a string...

  type NoStringTuple[T <: Tuple] = Tuple.Filter[T, IsNotString]

  val test1 : NoStringTuple[(Boolean, Int)] = (false,10) // OK
  // val test2 : NoStringTuple[(String, Int)] = ("Sample",10) // Not OK

  // This takes a tuple and returns a new one with all String types removed

  def noStringTuple[T <: Tuple](t: T): Tuple = {
    val unwrapped = t.productIterator.filter {
      case s: String => false
      case _ => true
    }.toArray[Any]
    Tuple.fromArray(unwrapped).asInstanceOf[Tuple]
  }

  println("No string tuple " + noStringTuple(("Hello", 1000, "Goodbye", 24L)))

  //import scala.deriving.Mirror
  // inline def derived[T](using m: Mirror.of[T]) = {

  //   ???
  // }

  import scala.compiletime.{constValue,constValueTuple}
  import scala.deriving.Mirror

  // get the type label from a mirror
  inline def labelFromMirror[A](using m: Mirror.Of[A]): String = {
    val s1 = constValue[m.MirroredLabel]
    val s2 = constValueTuple[m.MirroredElemLabels]
    s1 + s2
  }

  case class User(name: String, yob: Int)

  println(labelFromMirror[User]) // prints User

  // Play1
  //Macros.play1(101)





