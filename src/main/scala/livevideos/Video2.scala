package livevideos

import scala.math.{Numeric => _}

// Coded during video about type classes in Scala2
object Video2 extends App {
  
  object Scala2Numeric {

    object Numeric {
      def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric
    }
    
    trait Numeric[T] {
      def add(a: T, b: T): T
      def mul(a: T, b: T): T
      def square(a: T): T = mul(a, a)
    }

    object ops {

      implicit class NumericOps[T](a: T)(implicit numeric: Numeric[T]) {
        def add(b: T): T = numeric.add(a, b)
        def mul(b: T): T = numeric.mul(a, b)

        def +(b: T): T = add(b)
        def *(b: T): T = mul(b)
      }

    }

    implicit val intNumeric: Numeric[Int] = new Numeric[Int] {
      def add(a: Int, b: Int): Int = a + b

      def mul(a: Int, b: Int): Int = a * b

    }

    implicit val stringNumeric: Numeric[String] = new Numeric[String] {
      def add(a: String, b: String): String = a + b

      def mul(a: String, b: String): String = for (
        as <- a;
        bs <- b;
        s <- as.toString ++ bs.toString) yield s

    }
  }

  import Scala2Numeric._
  import Scala2Numeric.ops._

  def sumList[T : Numeric](ts: List[T]): T = {
    ts.reduce((a, b) => Numeric[T].add(a,b))
  }

  {
    val l1 = List(1, 2, 3, 4)
    val sum = sumList(l1)

    println(s"sum of int list is $sum")
  }

  {
    val s1 = "abcd"
    val s2 = "efgh"
    val product = s1 * s2
    println(s"product $product")
  }
  
  {
    val aPlusB = Numeric[Int].add(3,4)
    val aPlusB2 = 10.add(20)
    val aPlusB3 = "apple" * "blueberry"
    
    println(s"aPlusB $aPlusB aPlusB2 $aPlusB2 aPlusB3 $aPlusB3")
  }
}
