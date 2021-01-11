package livevideos

object Video6 extends App {
  
  import org.justinhj.typeclasses.functor.{_, given}

  // Recap Monad Functor and Applicative, they all have instances for either and list
  // can talk about the class arrangement and how givens are imported, and that we
  // can bring in functor or monad or applicative depending on the needs
  
  //import org.justinhj.typeclasses.{given Monad}
  //import org.justinhj.typeclasses.given_eitherMonad_Err

  // Testing the Functor laws

  // https://wiki.haskell.org/Functor
  
  // Identity
  
  // Applying the identity function to an effect does not change the effect...
  
  val l1 = List(1,2,3)

  def f(a: Int): Int = a + 1
  def g(a: Int): Int = a * 1

  val functorIdentityLawList = l1.fmap(identity) == l1
  println(s"functorIdentityLawList is $functorIdentityLawList ${l1.fmap(identity)}")
  
  val e1: Either[String, Integer] = Right(10)
  
  val functorIdentityLawEither = e1.fmap(identity) == e1
  println(s"functorIdentityLawEither is $functorIdentityLawEither")

  // Associativity law 
  
  


}
