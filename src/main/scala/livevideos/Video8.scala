package livevideos

object Video8 extends App:

  trait Semigroup[A]:
    extension(l:A) 
      def combine(r:A):A
      def |+|(r:A):A = l.combine(r)

  trait Monoid[A] extends Semigroup[A]:
    def zero: A

  object Monoid:
    def apply[A](using m: Monoid[A]) = m

  // Instances

  given intMonoid: Monoid[Int] with
    def zero = 0
    extension (l: Int) def combine(r:Int) = l + r

  // Usage

  println(10 |+| 20 |+| 30)
  
  // Left and right identity

  println(10 |+| Monoid[Int].zero)
  println(Monoid[Int].zero |+| 10)

  // Associative combination

  println((10 |+| 20) |+| 30)
  println(10 |+| ((20) |+| 30))

  object MultInts:
  
    opaque type MultInt = Int 
    
    object MultInt:
      def apply(a: Int): MultInt  = a
    
    given intMonoidMult: Monoid[MultInt] with
      def zero = 1
      extension (l: MultInt) def combine(r:MultInt) = l * r

  import MultInts.{given, _}

  val mi10 = MultInt(10)
  val mi20 = MultInt(20)

  println(mi10 |+| mi20)
  
  def sumList(as: List[Int]): Int = as.foldLeft(0){
    case (acc, a) =>
      acc + a
  }

  val l1 = List(1,2,3,4,5)
  println(sumList(l1))

  def fold[A: Monoid](as: List[A]): A = { 
    val m = Monoid[A]
    as.foldLeft(m.zero){
      case (acc, a) =>
        acc.combine(a)
    }
  }

  val l2 = List(1,2,3,4,5)
  println(fold(l2))

  def foldMap[A, B: Monoid](as: List[A], f: A => B): B = {
    val m = Monoid[B]
    as.foldLeft(m.zero){
      case (acc, a) =>
        acc.combine(f(a))
    }
  }

  val l3 = List(1,2,3,4,5)
  println(foldMap(l2, MultInt.apply))

  // Nested monoids - appending maps

  given mapMonoid[K, V: Monoid]: Monoid[Map[K,V]] with
    val vm = summon[Monoid[V]]
    def zero = Map.empty[K,V]
    extension(l: Map[K,V]) def combine(r: Map[K,V]) =
      (l.keys ++ r.keys).foldLeft(Map.empty[K,V]) {
        case (acc, k) =>
          acc.updated(k, l.getOrElse(k, vm.zero).combine(r.getOrElse(k,vm.zero)))
      }

  val m1 = Map("a" -> MultInt(30), "b" -> MultInt(20))
  val m2 = Map("b" -> MultInt(3), "c" -> MultInt(90))

  println(m1 |+| m2)


