import org.justinhj.typeclasses.applicative.{given, _}
import org.justinhj.typeclasses.numeric.{given, _}

object Scala3EvalEither extends App:

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound

  type EvalResult[A] = Either[EvalError, A]

  // Implement Numeric for EvalResult
  given evalResultNumeric[A: Numeric]: Numeric[Either[EvalError, A]] with {
    def add(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      
      a.map2(b)((a,b) => a + b)
      
//      
//      a.fflatMap {
//        aa => 
//          b.map {
//            bb =>
//              aa + bb
//          }
//      }
    }
    def mul(a: EvalResult[A], b: EvalResult[A]): EvalResult[A] = {
      
      a.map2(b)((a,b) => a * b)
      
//      a.fflatMap {
//        aa => 
//          b.map {
//            bb =>
//              aa * bb
//          }
//      }
    }
  }

  enum Exp[A]:
    case Val(value: A) extends Exp[A]
    case Add(left: Exp[A], right: Exp[A]) extends Exp[A]
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

  def handleAdd[A : Numeric](l: Exp[A] , r: Exp[A] ): WithEnv[A] = eval(l) + eval(r)
  
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
