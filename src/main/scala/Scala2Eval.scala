object Scala2Eval extends App {

  // An expression evaluator using Scala 2

  sealed trait Exp
  case class Val(value: Int) extends Exp
  case class Add(left: Exp, right: Exp) extends Exp
  case class Mul(left: Exp, right: Exp) extends Exp
  case class Var(identifier: String) extends Exp

  type Env = Map[String, Int]

  def eval(exp: Exp)(implicit env : Env): Int = {
    exp match {
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l,r) => handleAdd(l,r)
      case Mul(l,r) => handleMul(l,r)
    }
  }

  def handleAdd(l: Exp, r: Exp)(implicit env : Env) = eval(l) + eval(r)
  def handleMul(l: Exp, r: Exp)(implicit env : Env) = eval(l) * eval(r)
  def handleVar(s: String)(implicit env: Env) = env.getOrElse(s, 0)
  
  val exp1 : Exp = Mul(Var("z"), Add(Val(30), Mul(Var("x"), Var("y"))))
  
  implicit val env : Env = Map("x" -> 17, "y" -> 10, "z" -> 2)
  val eval1 = eval(exp1)

  println(s"Eval exp gives $eval1")
}
