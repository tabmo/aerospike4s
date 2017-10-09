package io.aerospike4s.decoder

import scala.collection.generic.CanBuildFrom

import cats.Applicative
import io.aerospike4s.{AsArray, AsLong, AsNull, AsObject, AsString, AsValue}

object DecoderYoloInterpreter { self =>

  import cats.instances.function._

  type Stack[A] = AsValue => A

  private val interpreter = new DecoderAlgebra[Stack] {
    override def field[A](field: String)(next: Decoder[A]): Stack[A] = {
      case o@AsObject(map) => o.get(field) match {
        case Some(f) => self.decodeUnsafe(next)(f)
        case None => throw ParseException(s"$field not present on object with fields ${map.keys.mkString(",")}.")
      }
      case e => throw ParseException(s"Try to parse object but $e is not.")
    }

    override def at[A](idx: Int)(next: Decoder[A]): Stack[A] = {
      case arr@AsArray(_) => arr.at(idx) match {
        case Some(v) => self.decodeUnsafe(next)(v)
        case None => throw ParseException(s"No value in array on index $idx.")
      }
      case e => throw ParseException(s"Try to parse array but $e is not.")
    }

    override def opt[A](next: Decoder[A]): Stack[Option[A]] = {
      case AsNull => None
      case v: AsValue => Some(self.decodeUnsafe(next).apply(v))
    }

    override def readString: Stack[String] = {
      case AsString(s) => s
      case e => throw ParseException(s"Try to parse a string value but $e is not.")
    }

    override def readLong: Stack[Long] = {
      case AsLong(l) => l
      case e => throw ParseException(s"Try to parse a long value but $e is not.")
    }

    override def readNull: Stack[Unit] = _ => ()

    override def readValues[A, L[A]](next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Stack[L[A]] = {
      case AsArray(arr) => {
        val builder = cbf.apply()
        arr.foreach(e => builder += self.decodeUnsafe(next).apply(e.value))
        builder.result()
      }
      case e => throw ParseException(s"Try read an array but $e is not.")
    }

    override def readFields[A](next: Decoder[A]): Stack[Map[String, A]] = {
      case AsObject(map) => map.mapValues(e => self.decodeUnsafe(next).apply(e.value))
      case e => throw ParseException(s"Try read an object but $e is not.")
    }

    override def pure[A](x: A): Stack[A] = Applicative[Stack].pure(x)

    override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]): Stack[B] = Applicative[Stack].ap(ff)(fa)
  }

  def decodeUnsafe[A](prog: Decoder[A]): Stack[A] = prog(interpreter)
}
