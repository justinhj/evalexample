package livevideos

// Numeric type class in Scala 3, live coded version 
object Video4 extends App {
  
  trait Numeric[T] {
    def add(a: T, b: T): T

    def mul(a: T, b: T): T

    extension (a: T) {
      def square: T = mul(a, a)
      def +(b: T) = add(a,b)
      def *(b: T) = mul(a,b)
    }
  }

  given Numeric[Int] with {
    def add(a: Int, b: Int): Int = a + b
    def mul(a: Int, b: Int): Int = a * b
  }

  given Numeric[String] with {
    def add(a: String, b: String): String = a ++ b
    def mul(a: String, b: String): String = {
      for (as <- a;
           bs <- b;
           s <- as.toString ++ bs.toString) yield s
    }

  }

  {
    val v1 = summon[Numeric[String]].square("ab")
    println(v1)
    
    val v2 = "abcd" * "efgh"
    println(v2)
    
    val v3 = 10.square
    println(v3)
  }
  
}