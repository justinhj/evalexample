object Scala2Numeric {

  object Numeric {
    // This can be used to summon a numeric (same as implicitly)
    def apply[T](implicit numeric: Numeric[T]): Numeric[T] = numeric

    object ops {

      implicit class NumericOps[T](a: T)(implicit n: Numeric[T]) {
        def add(b: T): T = n.add(a, b)

        def +(b: T): T = n.add(a, b)

        def mul(b: T): T = n.mul(a, b)

        def *(b: T): T = n.mul(a, b)
      }

    }

  }
  
  trait Numeric[T] {
    def add(a: T, b: T): T

    def mul(a: T, b: T): T

    def square(a: T): T = mul(a, a)
  }

  implicit val numericInt: Numeric[Int] = new Numeric[Int] {

    def add(a: Int, b: Int): Int = a + b

    def mul(a: Int, b: Int): Int = a * b
  }

  implicit val numericLong: Numeric[Long] = new Numeric[Long] {

    def add(a: Long, b: Long): Long = a + b

    def mul(a: Long, b: Long): Long = a * b
    
  }

  implicit val numericString: Numeric[String] = new Numeric[String] {

    def add(a: String, b: String): String = a ++ b

    def mul(a: String, b: String): String = {
      for (as <- a;
           bs <- b;
           s <- as.toString ++ bs.toString) yield s
    }

  }
}