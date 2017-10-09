package io.aerospike4s.syntax

import scala.util.Try

import io.aerospike4s.AsValue
import io.aerospike4s.decoder.{BinResolverInterpreter, Decoder, DecoderYoloInterpreter}

trait DecoderSyntax {

  implicit class DecoderOps[A](decoder: Decoder[A]) {
    def runEither(value: AsValue): Either[Throwable, A] = Try {
      DecoderYoloInterpreter.decodeUnsafe(decoder)(value)
    }.toEither

    def getBins: Seq[String] = BinResolverInterpreter.getBins(decoder)
  }
}
