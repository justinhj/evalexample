import scala.quoted._

trait Show[A]:
  def show(a: A): A

// This is the external interface to derive a show for any type
// Note the splice is needed because deriveShowImpl returns an Expr which 
// needs to be spliced into the code with an inline
inline def deriveShow[T]: Show[T] = ${ deriveShowImpl[T] }

// Question: what is the difference between QuoteContext and Quotes?
// note how we bring the type in implicitly as Type[T]
// 
def deriveShowImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[Show[T]] =
  import quotes.reflect._
  ???
  
  // val tpeSym = tpe.   //unseal.symbol
  // if tpeSym.flags.is(Flags.Case) then deriveCaseClassShow[T]
  // else if tpeSym.flags.is(Flags.Trait & Flags.Sealed) then deriveTraitShow[T]
  // else throw RuntimeException(s"Unsupported combination of flags: ${tpeSym.flags.show}")
