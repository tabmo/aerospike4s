package io.aerospike4s.syntax

import scala.concurrent.{ExecutionContext, Future}

import io.aerospike4s.{AerospikeIO, AerospikeManager, KleisliInterpreter}

trait AerospikeIOSyntax {

  implicit class AerospikeIOOps[A](io: AerospikeIO[A]) {

    def runFuture(manager: AerospikeManager)(implicit ec: ExecutionContext): Future[A] = {
      KleisliInterpreter.apply(ec).apply(io)(manager)
    }
  }

}
