package livevideos

import org.justinhj.typeclasses.functor.{given, _}

object Video6 extends App {

  val l1 = List(1,2,3)
  
  val functorIdentityLawList = l1.fmap(identity) == l1
  println(s"functorIdentityLawList == $functorIdentityLawList")

  def f(a: Int): Int = a + 1
  def g(a: Int): Int = a - 1
  
  val functorCompositionLawList = l1.fmap(f).fmap(g) == l1.fmap(a => g(f(a)))
  println(s"functorCompositionLawList == $functorCompositionLawList")

  
}
