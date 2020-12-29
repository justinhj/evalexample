object Scala3Numeric extends App {
  
  // Extension methods allow one to add methods to a type after the type is defined
  // Which is the essence of type classes!

  // Numeric type class in Scala 3
  trait Numeric[T] {
    def add(a: T, b: T): T
    def mul(a: T, b: T): T
    
    extension (a: T) {
      def +(b: T): T = add(a, b)
      def *(b: T): T = mul(a, b)
      def square: T = mul(a, a)
    }
  }
  
  given Numeric[Int] with {
    def add(a: Int, b: Int): Int = a + b
    def mul(a: Int, b: Int): Int = a * b
  }

  given Numeric[String] with {
    def add(a: String, b: String): String = a + b
    def mul(a: String, b: String): String = {
      for (
        as <- a;
        bs <- b;
        s <- as.toString ++ bs.toString
      ) yield s
    }
  }

  val val1 = summon[Numeric[Int]].add(10,20)
  println(s"10 + 20 = $val1")

  val val2 = "ab" * "cd"
  println(s"ab * cd = $val2")
}