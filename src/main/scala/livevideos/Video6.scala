package livevideos

import org.justinhj.typeclasses.functor.{given, _}

object Video6 extends App {

  // Identity law for functors...
  val l1 = List(1, 2, 3)
  println(l1 == l1.map(a => identity(a)))

  val e1: Either[String, Int] = Right(10)
  println(e1 == e1.map(identity))

  def f(a: Int): Int = a + 1

  def g(a: Int): Int = a - 1

  println(e1.map(f).map(g) == e1.map(a => g(f(a))))
  println(l1.map(f).map(g) == l1.map(a => g(f(a))))
}