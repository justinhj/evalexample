object ReaderPlay extends App:

  import org.justinhj.typeclasses.monad.{eitherMonad,_}
  import org.justinhj.typeclasses.numeric.{given, _}

  // ReaderT data type
  // Mostly from https://github.com/scalaz/scalaz/blob/80ba9d879b4f80f0175b5f904ac4587b02400251/core/src/main/scala/scalaz/Kleisli.scala

  case class ReaderT[F[_],R,A](run: R => F[A])

  object ReaderT:
    def lift[F[_],R,A](fa: F[A]): ReaderT[F,R,A] = ReaderT(_ => fa)

  // Monad instance

  given readerTMonad[F[_] : Monad,R]: Monad[[A1] =>> ReaderT[F,R,A1]] with
    def pure[A](a: A): ReaderT[F,R,A] = ReaderT(_ => Monad[F].pure(a))

    extension [A,B](far: ReaderT[F,R,A])
      def flatMap(f: A => ReaderT[F,R,B]) = {
        ReaderT((r: R) => {
          val fa: F[A] = far.run(r)
          val fb: F[B] = fa.flatMap(b => f(b).run(r))
          fb
      })
    }

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  type EvalResult[A] = ReaderT[[A] =>> Either[EvalError, A], Env[A], A]

  // Implement Numeric
  given evalResultNumeric[A: Numeric]: Numeric[EvalResult[A]] with {

    def isZero(a: EvalResult[A]): Boolean = {
      val what = a.run(Map.empty[String,A])
      what match {
        case Right(a) if summon[Numeric[A]].isZero(a) => true
        case _ => false
      }
    }

    def add(fa: EvalResult[A], fb: EvalResult[A]): EvalResult[A] = {
      fa.map2(fb)((a,b) => a + b)
    }

    def div(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      if isZero(b) then
        ReaderT.lift(Left(EvalError.DivisionByZero))
      else
        a.map2(b)(_ / _)
    }

    def sub(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      a.map2(b)((a, b) => a - b)
    }

    def mul(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      a.map2(b)((a,b) => a * b)
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

  type RResult[A] = ReaderT[[A1] =>> Either[EvalError, A1], Env[A], A]

  def eval[A : Numeric](exp: Exp[A]): RResult[A] =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => ReaderT.lift(Right(value))
      case Add(l,r) => handleAdd(l,r)
      case Sub(l,r) => handleSub(l,r)
      case Div(l,r) => handleDiv(l,r)
      case Mul(l,r) => handleMul(l,r)

  def handleAdd[A : Numeric](l: Exp[A] , r: Exp[A] ): RResult[A] = eval(l) + eval(r)
  def handleSub[A : Numeric](l: Exp[A] , r: Exp[A] ): RResult[A] = eval(l) - eval(r)
  def handleMul[A : Numeric](l: Exp[A] , r: Exp[A] ): RResult[A] = eval(l) * eval(r)
  def handleDiv[A : Numeric](l: Exp[A] , r: Exp[A] ): RResult[A] = eval(l) / eval(r)

  def handleVar[A](s: String): RResult[A] =
    ReaderT((env: Env[A]) =>
      env.get(s) match {
        case Some(value) => Right(value)
        case None => Left(EvalError.SymbolNotFound)
      })

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
    val env1: Env[Int] = Map("x" -> 1, "y" -> 10, "z" -> 100)
    val exp1 = Add(Mul(Val(10), Var("y")),Var("z"))
    println(eval(exp1).run(env1)) // == Right(100))

    val exp2 = Div(Val(1000), Var("z"))
    println(eval(exp2).run(env1)) // == Right(10))
  }

  {
    // Division by zero
    given envMap: Env[Int] = Map.empty
    val expO1 = Div(Val(10), Val(0))
    println(eval(expO1).run(envMap))
  }
