package livevideos

object Video8Prep extends App {
  
  // Implement semigroup type class
  // https://dotty.epfl.ch/docs/reference/contextual/type-classes.html
  
  trait Semigroup[A]:
    extension(l: A) 
      def combine(r: A): A
      def |+|(r: A): A = l.combine(r) 
    

  trait Monoid[A] extends Semigroup[A]:
    def zero:A
  
  // Define for int
  
  given intMonoid: Monoid[Int] with
    def zero = 0
    extension(l: Int) def combine(r: Int):Int = l + r
    
  
  // Use it 
  
  println(10.combine(20) == (10 |+| 20)) 
  println((10 |+| 20))
  // How about multiply?

//  given intMonoidMultiply: Monoid[Int] with
//    def zero = 1
//    extension(l: Int) def combine(r: Int):Int = l * r

  // Ambiguous!
  
  object MultInts {

    opaque type MultInt = Int

    object MultInt:
      def apply(a: Int): MultInt = a

    given intMonoidMultiply: Monoid[MultInt] with
      def zero = 1
      extension(l: MultInt) def combine(r: MultInt):MultInt = l * r
    
    // https://dotty.epfl.ch/docs/reference/other-new-features/opaques.html
    // In general, one can think of an opaque type as being only transparent in the scope of private[this].
  }

  import MultInts.{given, _}

  val mi10 = MultInt(10)
  val mi20 = MultInt(20)
  
  println(mi10.combine(mi20) == (mi10 |+| mi20))

  println(mi10 |+| mi20)
  
  // Composing on top of Monoid
  
  def sumList(as: List[Int]) = as.foldLeft(0){
    case (acc, a) =>
      acc + a
  }

  val l1 = List(1,2,3,4,5)
  println(sumList(l1))
  
  trait Foldable[F[_]]:
    extension [A: Monoid](as: F[A]) def ffold(): A
    extension [A, B: Monoid](as: F[A]) def foldMap(f: A => B): B

  given foldableList: Foldable[List] with
    extension [A: Monoid](as: List[A]) def ffold(): A = { 
      val m = summon[Monoid[A]]
      as.foldLeft(m.zero){case (acc,a) => acc.combine(a)}
    }
    extension [A, B: Monoid](as: List[A]) def foldMap(f: A => B): B =
      val m = summon[Monoid[B]]
      as.foldLeft(m.zero){case (acc,a) => acc.combine(f(a))}

  println(l1.ffold())
  println(l1.foldMap(MultInt.apply))
  
  // Nested monoids
  
  given mapMonoid[K,V: Monoid]: Monoid[Map[K,V]] with
    val vm = summon[Monoid[V]]
    def zero = Map.empty[K,V]
    extension(l: Map[K,V]) def combine(r: Map[K,V]): Map[K,V] =
      (l.keys ++ r.keys).foldLeft(Map.empty[K,V]) {
        case (acc, k) =>
          acc.updated(k, l.getOrElse(k,vm.zero).combine(r.getOrElse(k,vm.zero)))
      }
  
  val m1: Map[String, MultInt] = Map("a" -> MultInt(30), "b" -> MultInt(20))
  val m2: Map[String, MultInt] = Map("b" -> MultInt(3), "c" -> MultInt(90))

  println(m1 |+| m2)
}