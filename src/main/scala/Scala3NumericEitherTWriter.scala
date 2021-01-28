import org.justinhj.typeclasses.monad.{given, _}
import org.justinhj.typeclasses.numeric.{given, _}
import org.justinhj.datatypes.WriterT

object Scala3EvalEitherTWriter extends App {

  // Monad transformers are data structures that wrap an underlying value and provide
  // an effect
  //
  // Motivating example is we want to use Numeric[Either[EvalError,A]] as well as
  // add a log of our computation
  //
  // The effect of logging is captured with a Writer and to use Writer with our Either
  // we use the WriterT monad transformer
  //
  // source 7.4 in FP for mortals
  //
  // If we have some effect F[A] we can lift it to a transformer with liftM
  // def liftM[T[_[_],_], F[_]: Monad, A](fa: F[A]): T[F,A]
  //
  // and Hoist (advanced) for a natural transformation
  //
  // transformers generally implement convenient constructors on their companion
  // to make them easier to use

  // implement the Writer monad transformer

  // as an aside we need monoid, coming soon!

//  case class WriterT[F[_]: Monad,W,A](val wrapped: F[(W,A)])

  // Let's combine Writer with Either

  type EString[A] = Either[String,A]

  def incrementEven(a: Int): WriterT[EString,List[String],Int] = {
    if(a % 2 == 1) WriterT(Left("Odd number provided"))
    else WriterT(Right((List("Inc even"), a + 1)))
  }

  def doubleOdd(a: Int): WriterT[EString, List[String], Int] = {
    if(a % 2 == 0) WriterT(Left("Even number provided"))
    else WriterT(Right((List("Double odd"), a + a)))
  }

  // Pure
  
 // val pureWriterT = Monad[[A] =>> WriterT[EString, List[String], A]].pure(10)
   val butts: WriterT[EString,  List[String], Int] = incrementEven(8)
   val ass = summon[Monad[[X] =>> WriterT[EString,  List[String], X]]].fflatMap(butts)(doubleOdd)

//  val ptest = Monad[WriterT[Option,String,Int]].pure(22)
//  val butts: WriterT[[A] =>> Either[String, A], String, Int] = incrementEven(10)
//  val ass = butts.fflatMap(doubleOdd)

  println(ass)

  //val t1: WriterT[Option, String, Int] = incrementEven(2).fflatMap(a => doubleOdd(a))


  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  type EvalResult[A] = Either[EvalError, A]

  // Implement Numeric for EvalResult
  given evalResultNumeric[A: Numeric]: Numeric[Either[EvalError, A]] with {

    def isZero(a: EvalResult[A]): Boolean = {
      a match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResult[A], fb: EvalResult[A]): EvalResult[A] = {
      fa.map2(fb)((a,b) => a + b)
    }

    def div(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      if isZero(b) then
        Left(EvalError.DivisionByZero)
      else
        a.map2(b)(_ / _)
    }

    def sub(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {

      a.map2(b)((a, b) => a - b)
    }

    def mul(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] =
      a.map2(b)((a,b) => a * b)
  }

  enum Exp[A]:
    case Val(value: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Sub(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Mul(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Div(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Var(identifier: String) extends Exp[A]

  type Env[A] = Map[String, A]

  import Exp._

  type WithEnv[A] = Env[A] ?=> Either[EvalError, A]

  def summonEnv[A] : Env[A] ?=> Env[A] = summon[Env[A]]

  def eval[A : Numeric](exp: Exp[A]): WithEnv[A] =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => Right(value)
      case Add(l,r) => handleAdd(l,r)
      case Sub(l,r) => handleSub(l,r)
      case Div(l,r) => handleDiv(l,r)
      case Mul(l,r) => handleMul(l,r)

  def handleAdd[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) + eval(r)
  def handleSub[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) - eval(r)
  def handleMul[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) * eval(r)
  def handleDiv[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) / eval(r)

  def handleVar[A](s: String): WithEnv[A] =
    summonEnv.get(s) match {
      case Some(value) => Right(value)
      case None => Left(EvalError.SymbolNotFound)
    }

  val exp1 : Exp[Int] = Add(Var("z"), Add(Val(10), Add(Var("x"), Var("y"))))

  // Provide an environment and eval the expression
  {
    given envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  // And again with a different environment and a missing symbol
  {
    given envMap: Env[Int] = Map("x" -> 17, "y" -> 10, "a" -> 2)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  {
    // Test some operations
    given envMap: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)
    val expO1 = Mul(Val(10), Var("y"))
    assert(eval(expO1) == Right(100))

    val expO2 = Div(Val(1000), Var("z"))
    assert(eval(expO2) == Right(10))

    val expO3 = Sub(Val(1000), Mul(Var("y"), Var("z")))
    assert(eval(expO3) == Right(0))
  }

  {
    // Division by zero
    given envMap: Env[Int] = Map.empty
    val expO1 = Div(Val(10), Val(0))
    assert(eval(expO1) == Left(EvalError.DivisionByZero))
  }
}
