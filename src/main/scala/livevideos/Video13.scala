package livevideos

import org.justinhj.typeclasses.monad._
import org.justinhj.typeclasses.monad.eitherMonad
import org.justinhj.typeclasses.applicative.writerTApplicative
import org.justinhj.typeclasses.monoid.{listMonoid,_}
import org.justinhj.typeclasses.numeric.{given, _}
import org.justinhj.datatypes.WriterT

object Video13 extends App:

  import Exp._
  import Video12._

  enum EvalError:
    case InvalidSymboName
    case SymbolNotFound
    case DivisionByZero

  def mapTell2[A,B,C,F[_],W](fa: WriterT[F,W,A],fb: WriterT[F,W,B],fabc: (A,B) => C,fabcw: (A,B,C) => W)
                            (using m: Monoid[W], f: Monad[F]): WriterT[F,W,C] = {
    val r = fa.wrapped.map2(fb.wrapped){
      case ((al,a),(bl,b)) =>
        val c = fabc(a,b)
        val w = fabcw(a,b,c)
        val prev = m.combine(al,bl)
        (m.combine(prev,w),c)
    }
    WriterT(r)
  }

  // Implement Numeric for EvalResult
  given evalResultNumeric[A: Numeric]: Numeric[
    WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]
    ] with {

    // Help with type inference by just summoning the applicative for ReaderT and using it explicitly  
    val M = writerTApplicative[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String]]

    def isZero(a: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): Boolean = {
      a.wrapped.run(Map.empty[String, A]) match {
        case Right(a1) if summon[Numeric[A]].isZero(a1._2) => true
        case _ => false
      }
    }

    def add(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      mapTell2(fa,fb,{
        case (a,b) =>
          a + b
      },
      {
        case (a,b,c) =>
          List(s"Added $a to $b ($c)")
      })
      // Without the fancy logging it would be as simple as this 
      // M.map2(fa)(fb)(_ + _) 
    }

    def div(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      if isZero(fb) then
        WriterT.lift(ReaderT.lift(Left(EvalError.DivisionByZero)))
      else
        mapTell2(fa,fb,{
          case (a,b) =>
            a / b
        },
        {
          case (a,b,c) =>
            List(s"Divided $a by $b ($c)")
        })
    }

    def sub(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      mapTell2(fa,fb,{
        case (a,b) =>
            a - b
        },
        {
          case (a,b,c) =>
            List(s"Subtracted $a from $b ($c)")
        })    
    }

    def mul(fa: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A], fb: WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A]): WriterT[[RA] =>>ReaderT[[EA] =>> Either[EvalError, EA], Env[A],RA],List[String],A] = {
      mapTell2(fa,fb,{
          case (a,b) =>
            a * b
        },
        {
          case (a,b,c) =>
            List(s"Multiplied $a by $b ($c)")
        })    
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
      case Val(value) => WriterT(ReaderT.lift(Right(List(s"Literal value $value"),value)))
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
  val exp1 : Exp[Int] = Mul(
                            Var("z"),
                            Add(
                              Sub(
                                Div(
                                  Val(10),
                                  Val(2)
                                ),
                                Val(2)
                              ),
                              Mul(
                                Var("x"),
                                Var("y"))))

  // Provide an environment and eval the expression
  {
    val envMap: Env[Int] = Map("x" -> 7, "y" -> 6, "z" -> 22)

    val eval1 = eval(exp1).wrapped.run(envMap)

    println(s"Eval exp gives $eval1")
  }

  // And again with a missing symbol
  {
    val envMap: Env[Int] = Map("x" -> 17, "y" -> 10, "a" -> 2)

    val eval1 = eval(exp1).wrapped.run(envMap)

    println(s"Eval exp gives $eval1")
  }