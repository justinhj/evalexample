package org.justinhj.typeclasses

trait Monad[F[_]] extends Functor[F]:

        /** The unit value for a monad */
        def pure[A](x:A):F[A]
        
        extension[A,B](x:F[A])
                /** The fundamental composition operation */
                def flatMap(f:A=>F[B]):F[B]
                
                /** The `map` operation can now be defined in terms of `flatMap` */
                def map(f:A=>B)=x.flatMap(f.andThen(pure))

end Monad
