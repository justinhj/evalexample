package org.justinhj.datatypes

import org.justinhj.typeclasses.monad.Monad

case class WriterT[F[_]: Monad,W,A](val wrapped: F[(W,A)])
