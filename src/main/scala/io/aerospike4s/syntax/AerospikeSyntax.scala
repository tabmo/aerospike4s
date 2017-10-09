package io.aerospike4s.syntax

import scala.concurrent.{ExecutionContext, Future}

import cats.data.Kleisli
import io.aerospike4s.{AerospikeIO, AerospikeManager, BaseInterpreter, KleisliInterpreter}

trait AerospikeIOSyntax {

  implicit class AerospikeIOOps[A](io: AerospikeIO[A]) {

    def runFuture(manager: AerospikeManager)(implicit ec: ExecutionContext): Future[A] = {
      import cats.implicits._
      BaseInterpreter[Kleisli[Future, AerospikeManager, ?]](
        KleisliInterpreter.apply(ec)
      ).apply(io)(manager)
    }
  }

}
