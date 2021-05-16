package livevideos

object Video13 extends App:

  // Steps, copied this from video 12
  // added the writerT import 
  // todo will need to move readerT to monad 
  // change the data type 


  import org.justinhj.typeclasses.monad._
  import org.justinhj.typeclasses.monad.eitherMonad
  import org.justinhj.typeclasses.monad.writerTMonad
  import org.justinhj.typeclasses.monoid.{listMonoid,_}
  import org.justinhj.typeclasses.numeric.{given, _}
  import org.justinhj.datatypes.WriterT

  case class ReaderT[F[_],R,A](run: R => F[A])

  object ReaderT:
    def lift[F[_],R,A](fa: F[A]): ReaderT[F,R,A] = ReaderT(_ => fa)

  given readerTMonad[F[_]: Monad,R]: Monad[[A1] =>> ReaderT[F,R,A1]] with
    def pure[A](a:A): ReaderT[F,R,A] = ReaderT(_ => Monad[F].pure(a))

    extension [A,B](far: ReaderT[F,R,A])
      def flatMap(f: A => ReaderT[F,R,B]): ReaderT[F,R,B] = {
        ReaderT((r: R) =>
          val fa: F[A] = far.run(r)
          val fb: F[B] = fa.flatMap(b => f(b).run(r))
          fb
        )
      }

  import Exp._
  import Video12._

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  // Implement Numeric for EvalResult
  given evalResultNumeric[A: Numeric]: Numeric[
    WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]
    ] with {

    // Could use applicative here
    val M = writerTMonad[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String]]

    def isZero(a: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): Boolean = {
      a.wrapped.run(Map.empty[String, A]) match {
        case Right(a1) if summon[Numeric[A]].isZero(a1._2) => true
        case _ => false
      }
    }

    def add(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      M.map2(fa)(fb)((a,b) => a + b)
    }

    def div(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      if isZero(fb) then
        WriterT.lift(ReaderT.lift(Left(EvalError.DivisionByZero)))
      else
        M.map2(fa)(fb)((a,b) => a / b)
    }

    def sub(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      M.map2(fa)(fb)((a,b) => a - b)
    }

    def mul(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      M.map2(fa)(fb)((a, b) => a * b)
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

  def eval[A : Numeric](exp: Exp[A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] =
    exp match
      case Var(id) => handleVar(id)
      case Val(value) => WriterT.lift(ReaderT.lift(Right(value)))
      case Add(l,r) => handleAdd(l,r)
      case Sub(l,r) => handleSub(l,r)
      case Div(l,r) => handleDiv(l,r)
      case Mul(l,r) => handleMul(l,r)

  def handleAdd[A : Numeric](l: Exp[A] , r: Exp[A] ): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = eval(l) + eval(r)
  def handleSub[A : Numeric](l: Exp[A] , r: Exp[A] ): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = eval(l) - eval(r)
  def handleMul[A : Numeric](l: Exp[A] , r: Exp[A] ): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = eval(l) * eval(r)
  def handleDiv[A : Numeric](l: Exp[A] , r: Exp[A] ): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = eval(l) / eval(r)

  def handleVar[A](s: String): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] =
    WriterT(ReaderT((env: Env[A]) =>
      env.get(s) match {
        case Some(value) => Right(List(s"Looked up var $s ($value)"),value)
        case None => Left(EvalError.SymbolNotFound)
    }))

  // A sample expression
  val exp1 : Exp[Int] = Add(
                            Var("z"),
                            Add(
                              Val(10),
                              Mul(
                                Var("x"),
                                Var("y"))))

  // Provide an environment and eval the expression
  {
    val envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1).value.run(envMap)

    println(s"Eval exp gives $eval1")
  }

  // And again with a missing symbol
  {
    val envMap: Env[Int] = Map("x" -> 17, "y" -> 10, "a" -> 2)

    val eval1 = eval(exp1).value.run(envMap)

    println(s"Eval exp gives $eval1")
  }

