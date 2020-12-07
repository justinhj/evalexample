object Scala2Eval extends App {

  // An expression evaluator using Scala 2

  import Scala2Numeric._
  import Scala2Numeric.Numeric.ops._
  
  sealed trait Exp[T]
  case class Val[T](value: T) extends Exp[T]
  case class Add[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class Mul[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class Var[T](identifier: String) extends Exp[T]

  type Env[T] = Map[String, T]

  def eval[T](exp: Exp[T])(implicit env : Env[T], numeric: Numeric[T]): T = {
    exp match {
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l,r) => handleAdd(l,r)
      case Mul(l,r) => handleMul(l,r)
    }
  }

  def handleAdd[T](l: Exp[T], r: Exp[T])(implicit env : Env[T], numeric: Numeric[T]): T = eval(l) + eval(r)
  def handleMul[T](l: Exp[T], r: Exp[T])(implicit env : Env[T], numeric: Numeric[T]): T = eval(l) * eval(r)
  def handleVar[T](s: String)(implicit env: Env[T], numeric: Numeric[T]): T = env(s)
  
  val exp1 : Exp[Int] = Mul(Var("z"), Add(Val(30), Mul(Var("x"), Var("y"))))
  
  implicit val env : Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)
  val eval1 = eval(exp1)
  println(s"Eval exp gives $eval1")

  implicit val env2 : Env[String] = Map("x" -> "aaaa", "y" -> "bbbb")
  val exp2: Exp[String] = Mul(Var("x"), Var("y"))
  val eval2 = eval(exp2)

  println(s"Eval exp gives $eval2")
}
