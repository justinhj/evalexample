import org.justinhj.typeclasses.monad.{given,_}
import org.justinhj.typeclasses.numeric.{given,_}
import org.justinhj.datatypes.WriterT
import org.justinhj.typeclasses.monoid.{given,_}

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

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  //type EvalResult[A] = WriterT[EString, List[String], A]

  type EvalResult[A] = Either[EvalError, A]
  
  type EvalResultW[A] = WriterT[[A1] =>> EvalResult[A1], List[String], A]
  
  type EString[A] = Either[String,A]

  def incrementEven(a: Int): EvalResult[Int] = {
    if(a % 2 == 1) Left(EvalError.SymbolNotFound)
    else Right(a + 1)
  }

  def doubleOdd(a: Int): EvalResult[Int] = {
    if(a % 2 == 0) Left(EvalError.DivisionByZero)
    else Right(a + a)
  }
  
  val m1 = summon[Monad[[A] =>> WriterT[[A1] =>> EvalResult[A1], List[String], A]]]
  
  // All valid ways to get a pure value as a writerT
  val pure8_2 = summon[Monad[[A] =>> WriterT[[A1] =>> EvalResult[A1], List[String], A]]].pure(8)
  val pure8_3 = Monad[[A] =>> WriterT[[A1] =>> EvalResult[A1], List[String], A]].pure(8)
  
  val pure8 = m1.pure(8).tell(List("OK")).tell(List("Let's go!"))
  println(pure8)

  // Lift example
  val liftedExample: WriterT[[A1] =>> EvalResult[A1], String, Int] = WriterT.lift(Right(8))
  val addedLog = liftedExample.tell("Added a log!")
  println("Just the log " + addedLog.written)

  val liftedLeftExample: WriterT[[A1] =>> EvalResult[A1], String, Int] = WriterT.lift(Left(EvalError.SymbolNotFound))
  println("Just the log " + liftedLeftExample.tell("Hello!").written)
  
  val example: WriterT[[A1] =>> EvalResult[A1], List[String], Int] =
    WriterT.lift[[A1] =>> EvalResult[A1], List[String], Int](incrementEven(8)).
      tellWith(a => List(s"Incremented to $a")).
      flatMap{
        a => 
          WriterT.lift[[A1] =>> EvalResult[A1], List[String], Int](doubleOdd(a))
      }.
      tellWith(a => List(s"Doubled to $a"))
  
  println(s"example $example")

  given evalResultWNumeric[A: Numeric]: Numeric[WriterT[[A1] =>> Either[EvalError, A1], List[String], A]] with
  {

    def isZero(fa: EvalResultW[A]): Boolean = {
      fa.value match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResultW[A], fb: EvalResultW[A]): EvalResultW[A] = {
      writerTMonad[[A1] =>> Either[EvalError, A1], List[String]].map2(fa)(fb)((a, b) => a + b).tellWith(a => List(s"add $a"))
    }

    def div(a: EvalResultW[A], b: EvalResultW[A]): EvalResultW[A] = {
      if isZero(b) then
        WriterT.lift(Left(EvalError.DivisionByZero))
      else
        writerTMonad[[A1] =>> Either[EvalError, A1], List[String]].map2(a)(b)(_ / _).tellWith(a => List(s"div $a"))
    }

    def sub(a: EvalResultW[A], b: EvalResultW[A]): EvalResultW[A] = {
      writerTMonad[[A1] =>> Either[EvalError, A1], List[String]].map2(a)(b)((a, b) => a - b).tellWith(a => List(s"sub to $a"))
    }

    def mul(a: EvalResultW[A], b: EvalResultW[A]): EvalResultW[A] = {
      a.map2(b)((a, b) => a * b).tellWith(a => List(s"mul to $a"))
    }
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
  type WithEnv[A] = Env[A] ?=> WriterT[[A1] =>> Either[EvalError, A1], List[String], A]

  def summonEnv[A] : Env[A] ?=> Env[A] = summon[Env[A]]

  def eval[A : Numeric](exp: Exp[A]): WithEnv[A] =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => WriterT.lift[[A1] =>> EvalResult[A1], List[String], A](Right(value)).tellWith(a => List(s"Val $a"))
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
      case Some(value) => WriterT.lift(Right(value))
      case None => WriterT.lift(Left(EvalError.SymbolNotFound))
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
    val expO1 = Mul(
      Val(10), 
      Add(
        Val(25),
        Var("y")))
      
    println(s"exp01 ${eval(expO1)}")
  }

  {
    // Division by zero
    given envMap: Env[Int] = Map.empty
    val expO1 = Div(Val(10), Val(0))
    println(s"exp01 ${eval(expO1)}")
  }
}
