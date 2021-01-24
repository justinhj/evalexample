object SumMacro {

  import scala.quoted._

  // Type[T] is needed so that we can use T in the code below
  // Whole expression is quoted 
  //
  def map[T](arr: Expr[Array[T]], f: Expr[T] => Expr[Unit])
            (using Type[T], Quotes): Expr[Unit] = '{
     var i: Int = 0
     while i < ($arr).length do // Note that we are in a quoted block so parameters are spliced $arr 
       val element: T = ($arr)(i) // 
       ${f('element)}
       i += 1
  }

  // Input is now an expression of the input type, output will be an Expression too  
  // we bring scala.quoted.Quotes in to do its magic 
  def sum(arr: Expr[Array[Int]])(using Quotes): Expr[Int] = '{ // Whole thing is quoted 
    var sum = 0
    ${ map(arr, x => '{sum += $x}) } // splice in the call to the map macro
    sum // return the value as normal
  }

  // As always note that the external interface to a macro is an inline function
  // that uses splice to bring it up to the right level and uses quote on the params 
  // for the same reason...
  inline def sum_m(inline arr: Array[Int]): Int = ${sum('arr)}

}
