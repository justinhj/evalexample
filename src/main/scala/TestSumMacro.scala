object TestSumMacro extends App {

  import SumMacro._

  val tm = sum_m(Array(1,2,3,4,5))

  println(s"Sum of array $tm")
}
