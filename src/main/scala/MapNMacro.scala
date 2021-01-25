object MapNMacro {

  import scala.quoted._
  import org.justinhj.typeclasses.applicative.{_}


//  def sequence[T <: Tuple](t: T): Option[Tuple.InverseMap[T, Option]] = {
//    val unwrapped = t.productIterator.collect {case Some(s) => s}.toArray[Any]
//    if(unwrapped.length == t.productArity)
//      Some(Tuple.fromArray(unwrapped).asInstanceOf[Tuple.InverseMap[T, Option]])
//    else
//      None
//  }
  
  def productMImpl(fs: Expr[Seq[Int]])(using Quotes): Expr[Int] = {
    import quotes.reflect._
    val tree: Term = fs.asTerm
    tree match {
      case Inlined(None, Nil, Typed(expr, tpt)) =>
        expr match {
          case Repeated(args, _) =>
          
            //report.error("victory " + what)
            val what: Int = args.collect{
              case Literal(IntConstant(n)) =>
                n
            }.sum
            '{${Expr(what)}}
          case expr =>
            report.error("poop " + expr.show(using Printer.TreeStructure))
            '{1}
        }
      
      case expr =>
        //println(Printer.TreeStructure.show(tree))
        report.error(tree.show(using Printer.TreeStructure))
        //report.error("Parameter must be a known constant")
        '{4}
    }
  }

/*
  Inlined(None, Nil, 
    Typed(
      Repeated(
        List(
          Literal(IntConstant(10)), 
          Literal(IntConstant(20)), 
          Literal(IntConstant(30))
          ), Inferred()), 
        Inferred()))
 */
  
  
  
//  println(s"productM test ${MapNMacro.productM(10, 20, 30)}")

  
  // Macro to take a sequence of of F[_] and return a F[Tuple] of the results
  // requires an applicative in scope for F
  inline def productM(inline fs: Int*): Int = 
    ${productMImpl('fs)}
  
//  def mapNMacro[F[_]: Applicative](efs: Expr[Seq[F[_]]])(using Type[F], Quotes): Expr[F[Seq[_]]] = '{
//    val app = summon[Applicative[F]]
//    app.ap($efs(0))($f)
//  }
//
//  inline def mapN[F[_]: Applicative,A](efs: F[A]*): F[A] = 
//    ${mapNMacro('efs)}

  // Temp crap

  // Type[T] is needed so that we can use T in the code below
  // Whole expression is quoted 
  //
  def map[T](arr: Expr[Seq[T]], f: Expr[T] => Expr[Unit])
            (using Type[T], Quotes): Expr[Unit] = '{
     var i: Int = 0
     while i < ($arr).length do // Note that we are in a quoted block so parameters are spliced $arr 
       val element: T = ($arr)(i) // 
       ${f('element)}
       i += 1
  }

  // Input is now an expression of the input type, output will be an Expression too  
  // we bring scala.quoted.Quotes in to do its magic 
  def sum(arr: Expr[Seq[Int]])(using Quotes): Expr[Int] = '{ // Whole thing is quoted 
    var sum = 0
    ${ map(arr, x => '{sum += $x}) } // splice in the call to the map macro
    sum // return the value as normal
  }

  // As always note that the external interface to a macro is an inline function
  // that uses splice to bring it up to the right level and uses quote on the params 
  // for the same reason...
  // var args version
  inline def sum_ms(inline seq: Int*): Int = ${ 
    sum('seq)
  }

  //inline def sum_m(inline arr: Array[Int]): Int = ${sum('arr)}

}
