object Scala3Eval extends App:

  enum Exp:
    case Val(value: Int) extends Exp
    case Add(left: Exp, right: Exp) extends Exp
    case Var(identifier: String) extends Exp
  
  type Env = Map[String, Int]
  
  import Exp._
  
  type WithEnv = Env ?=> Int
  
  def summonEnv : Env ?=> Env = summon[Env]
  
  def eval(exp: Exp): WithEnv =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l,r) => handleAdd(l,r)

  def handleAdd(l: Exp, r: Exp): WithEnv = eval(l) + eval(r)
  
  def handleVar(s: String): WithEnv =
    summonEnv.getOrElse(s, 0)

  val exp1 : Exp = Add(Var("z"), Add(Val(10), Add(Var("x"), Var("y"))))
  
  // Provide an environment and eval the expression
  {
    given envMap as Env = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }

  // And again with a different environment
  {
    given envMap as Env = Map("x" -> 17, "y" -> 10, "z" -> 2)

    val eval1 = eval(exp1)

    println(s"Eval exp gives $eval1")
  }