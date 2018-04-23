package io.aerospike4s.decoder

import scala.collection.generic.CanBuildFrom

object BinResolverInterpreter {
  type Stack[A] = List[String]

  private val interpreter = new DecoderAlgebra[Stack] {
    override def field[A](field: String)(next: Decoder[A]): Stack[A] = List(field)

    override def at[A](idx: Int)(next: Decoder[A]): Stack[A] =  List("value")

    override def opt[A](next: Decoder[A]): Stack[A] = getBins(next)

    override def readString: Stack[String] = List("value")

    override def readLong: Stack[Long] = List("value")

    override def readNull: Stack[Unit] = Nil

    override def readValues[A, L[_]](next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Stack[L[A]] = List("value")

    override def readRawValue = Nil

    override def readFields[A](next: Decoder[A]): Stack[Map[String, A]] = Nil

    override def pure[A](x: A): Stack[A] = Nil

    override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]): Stack[B] = ff ++ fa
  }

  def getBins[A](prog: Decoder[A]): Stack[A] = prog(interpreter)
}
