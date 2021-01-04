import org.justinhj.typeclasses._

object Scala3EvalEither extends App:

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound

  type EvalResult[A <: Numeric[A]] = Either[EvalError, A]

//  given evalResultNumeric[A: Numeric]: Numeric[EvalResult[A]] with {
//    def add[A](a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = ??? 
//    def mul[A](a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = ???
//  }

  enum Exp[A]:
    case Val(value: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
    case Var(identifier: String) extends Exp[A]
  
  type Env[A] = Map[String, A]
  
  import Exp._
  
  type WithEnv[A] = Env[A] ?=> A

  def summonEnv[A] : Env[A] ?=> Env[A] = summon[Env[A]]
  
  def eval[A](exp: Exp[A]): WithEnv[A] =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l,r) => handleAdd(l,r)

  def handleAdd[A](l: Exp[A] , r: Exp[A] ): WithEnv[A] = ??? // eval(l) + eval(r)
  
  def handleVar[A](s: String): WithEnv[A] =
    summonEnv(s)

  val exp1 : Exp[Int] = Add(Var("z"), Add(Val(10), Add(Var("x"), Var("y"))))
  
  // Provide an environment and eval the expression
  {
    given envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  // And again with a different environment
  {
    given envMap: Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }
