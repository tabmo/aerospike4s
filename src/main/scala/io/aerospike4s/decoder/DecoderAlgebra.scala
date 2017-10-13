package io.aerospike4s.decoder

import scala.collection.generic.CanBuildFrom

import cats.Applicative
import io.aerospike4s.AsValue

trait DecoderAlgebra[F[_]] extends Applicative[F] {
  def field[A](field: String)(next: Decoder[A]): F[A]

  def at[A](idx: Int)(next: Decoder[A]): F[A]

  def opt[A](next: Decoder[A]): F[Option[A]]

  def readString: F[String]

  def readLong: F[Long]

  def readNull: F[Unit]

  def readRawValue: F[AsValue]

  def readValues[A, L[_]](next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): F[L[A]]

  def readFields[A](next: Decoder[A]): F[Map[String, A]]
}