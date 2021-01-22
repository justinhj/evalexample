object TypeClassParser extends App {
    import scala.deriving._
    import scala.compiletime._

    case class ParseError(str: String, msg: String)

    // Decoder type class takes a string to a parse error or a valid
    // instance of T...
    trait Decoder[T]{
        def decode(str:String): Either[ParseError, T]
    }

    object Decoder {
        inline given stringDec: Decoder[String] = new Decoder[String] {
            override def decode(str: String): Either[ParseError, String] = Right(str)
        }

        inline given intDec: Decoder[Int] = new Decoder[Int] {
            override def decode(str: String): Either[ParseError, Int] =
            str.toIntOption.toRight(ParseError(str, "value is not valid Int"))
        }

        // Derives a decoder for any type T
        // The name derived is important. By convention it must be called that.
        // Mirror of T gives us access to compile time reflection
        inline def derived[T](using m: Mirror.Of[T]): Decoder[T] = {
                val elemInstances = summonAll[m.MirroredElemTypes]
                inline m match {
                    case p: Mirror.ProductOf[T] => productDecoder(p, elemInstances)
                    case s: Mirror.SumOf[T]     => ???
            }
        }

        inline def summonAll[T <: Tuple]: List[Decoder[_]] = inline erasedValue[T] match {
            case _: EmptyTuple => Nil
            case _: (t *: ts) => summonInline[Decoder[t]] :: summonAll[ts]
        }

        def productDecoder[T](p: Mirror.ProductOf[T], elems: List[Decoder[_]]): Decoder[T] =
            new Decoder[T] {
                def decode(str: String): Either[ParseError, T] = {
                    val what = elems.zip(str.split(','))
                    .map(_.decode(_).map(_.asInstanceOf[AnyRef]))

                    sequence(what)
                    .map(ts => p.fromProduct(new ArrayProduct(ts.toArray)))
                }
        }

        def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
            traverse(es)(identity)

        def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
            es.foldRight[Either[E, List[B]]](Right(Nil))((h, tRes) => map2(f(h), tRes)(_ :: _))

        def map2[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[E, C] =
            for { a1 <- a; b1 <- b } yield f(a1,b1)
        }

        // Derives Decoder tells the compiler it can use the derived definition
        // above to derive a Decoder from this case class...
        case class A(i: Int, s: String) derives Decoder

        println(summon[Decoder[A]].decode("10,justin"))//Right(A(10,abc))
        println(summon[Decoder[A]].decode("xxx,abc"))//Left(ParseError(xxx,value is not valid Int))
        // println(summon[Decoder[A]].decode(","))

}