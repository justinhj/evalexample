object ReaderWriterPlay extends App:

  import org.justinhj.typeclasses.monad.{eitherMonad,_}
  import org.justinhj.typeclasses.numeric.{given, _}

  // In this one we're going to use a Reader environment that includes
  // both the symbol table and a mutable log we can write to 
  
  // ReaderT data type
  // Mostly from https://github.com/scalaz/scalaz/blob/80ba9d879b4f80f0175b5f904ac4587b02400251/core/src/main/scala/scalaz/Kleisli.scala

  case class ReaderT[F[_],R,A](run: R => F[A]):
    // This lets you get at the environment 
    def ask(using m: Monad[F]): ReaderT[F,R,R] =
      ReaderT(r => m.pure(r))

    def local[RR](f: RR => R): ReaderT[F, RR, A] =
      ReaderT(f andThen run)
    
  // Companion object 

  object ReaderT:
    def lift[F[_],R,A](fa: F[A]): ReaderT[F,R,A] = ReaderT(_ => fa)
    def ask[F[_],R](using m: Monad[F]): ReaderT[F,R,R] = ReaderT(r => m.pure(r))
  
  // Monad instance

  given readerTMonad[F[_] : Monad,R]: Monad[[A1] =>> ReaderT[F,R,A1]] with
    def pure[A](a: A): ReaderT[F,R,A] = ReaderT(_ => Monad[F].pure(a))

    extension [A,B](fa: ReaderT[F,R,A])
      def flatMap(f: A => ReaderT[F,R,B]) =
        val r2ReaderFRB = (r: R) => fa.run(r).flatMap(b => f(b).run(r))
        ReaderT(r2ReaderFRB)

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

    val eval1 = eval(exp1).run(envMap)

    println(s"Eval exp gives $eval1")
  }

  // And again with a different environment and a missing symbol
  {
    given envMap: Env[Int] = Map("x" -> 17, "y" -> 10, "a" -> 2)

    val eval1 = eval(exp1).run(envMap)

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

  // A simple example 

  case class DbConfig(url: String, user: String)
  case class ServiceConfig(name: String)
  case class AppConfig(dbConfig: DbConfig, serviceConfig: ServiceConfig)

  val sampleConfig = AppConfig(DbConfig("db.com", "root"),ServiceConfig("Groot"))
  
  // A pretend DB op
  def writeDB(key: String, value: Int):  ReaderT[[A] =>> Either[String,A], DbConfig, Unit] =
    ReaderT(config =>
      Right(println(s"Writing $key:$value to DB at ${config.url}/${config.user}")))

  // A pretend Service op
  def startService():  ReaderT[[A] =>> Either[String,A], ServiceConfig, Unit] =
    ReaderT(config =>
      Right(println(s"Service ${config.name} started")))
  
  val m = readerTMonad[[A] =>> Either[String,A],DbConfig]
  val pureTest = m.pure(10)
  val envTest = pureTest.ask.run(DbConfig("db.com", "root"))
  println(s"envTest $envTest")

  val pureTestInfer = Monad[[A1] =>> ReaderT[[A] =>> Either[String,A], DbConfig, A1]].pure(10)
  val envTestInfer = pureTestInfer.ask.run(DbConfig("db.com", "root"))
  println(s"envTestInfer $envTestInfer")

  // try out ask and local
  val intReader = Monad[[A1] =>> ReaderT[[A] =>> Either[String,A], Int, A1]].pure(10)
  val intReaderAsk = intReader.ask.run(22)
  // local lets us change the global environnment to a local one for a computation
  // here the global is a string and our computation wants an integer
  val intReaderLocal = intReader.local[String](a => a.toInt + 1).run("22")
  
  println(s"intReaderAsk $intReaderAsk intReaderLocal $intReaderLocal")
  
  type StringEither[A] = Either[String,A] 
  
  def prog(key: String, value: Int): ReaderT[StringEither, AppConfig, Unit] = for (
    config <- ReaderT.ask[StringEither, AppConfig];
    _ = println(s"Config is $config!");
    _ <- startService().local[AppConfig](_.serviceConfig);
    ok <- writeDB(key,value).local[AppConfig](_.dbConfig);
    ok2 <- writeDB(key + "2", value).local[AppConfig](_.dbConfig)
  ) yield (ok)

  println(prog("justin", 100).run(sampleConfig))



