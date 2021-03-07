import org.justinhj.typeclasses.monad.{given,_}
import org.justinhj.typeclasses.numeric.{given,_}
import org.justinhj.datatypes.WriterT
import org.justinhj.typeclasses.monoid.{given,_}

object Scala3EvalEitherTWriter extends App {

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero
  
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

  def mapTell2[A,B,C,F[_],W](fa: WriterT[F,W,A],fb: WriterT[F,W,B],fabc: (A,B) => C,fabcw: (A,B,C) => W)
                            (using m: Monoid[W], f: Monad[F]): WriterT[F,W,C] = {
    val r = fa.wrapped.map2(fb.wrapped){
      case ((al,a),(bl,b)) =>
        val c = fabc(a,b)
        val w = fabcw(a,b,c)
        val prev = m.combine(al,bl)
        (m.combine(w,prev),c)
    }
    WriterT(r)
  }

  given evalResultWNumeric[A: Numeric]: Numeric[WriterT[[A1] =>> Either[EvalError, A1], List[String], A]] with

    def isZero(fa: EvalResultW[A]): Boolean = {
      fa.value match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResultW[A], fb: EvalResultW[A]): EvalResultW[A] = {
      mapTell2(fa,fb,(a, b) => a + b,(a,b,c) => List(s"$c: added $a to $b"))
    }

    def div(a: EvalResultW[A], b: EvalResultW[A]): EvalResultW[A] = {
      if isZero(b) then
        WriterT.lift(Left(EvalError.DivisionByZero))
      else
        mapTell2(a,b,(a, b) => a / b,(a,b,c) => List(s"$c: divided $a by $b"))
    }

    def sub(a: EvalResultW[A], b: EvalResultW[A]): EvalResultW[A] = {
      mapTell2(a,b,(a, b) => a / b,(a,b,c) => List(s"$c: subtracted $a from $b"))
    }

    def mul(a: EvalResultW[A], b: EvalResultW[A]): EvalResultW[A] = {
      mapTell2(a,b,(a, b) => a * b,(a,b,c) => List(s"$c: multiplied $a by $b"))
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
      case Var(id) => handleVar(id).tellWith(a => List(s"Var $id $a"))
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

  val exp1 : Exp[Int] = 
    Add(
      Var("z"), 
      Add(
        Val(10), 
        Add(
          Var("x"), 
          Var("y"))))

  // Provide an environment and eval the expression
  {
    given envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1)

    eval1.written match {
      case Right(log) =>
        log.foreach(println)
      case Left(err) => ()
    }
    
  }
  

  {
    // Division by zero
    given envMap: Env[Int] = Map.empty
    val expO1 = Div(Val(10), Val(0))
    println(s"exp01 ${eval(expO1)}")
  }
}
