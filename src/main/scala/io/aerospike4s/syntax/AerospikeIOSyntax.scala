package io.aerospike4s.syntax

import scala.concurrent.ExecutionContext

import cats.effect.Async
import io.aerospike4s.{AerospikeIO, AerospikeManager, KleisliInterpreter}

trait AerospikeIOSyntax {

  implicit class AerospikeIOOps[A](io: AerospikeIO[A]) {

    def run[F[_]](manager: AerospikeManager)(implicit ec: ExecutionContext, asyncF: Async[F]): F[A] = {
      KleisliInterpreter.apply[F].apply(io)(manager)
    }
  }

}
