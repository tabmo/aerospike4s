package io.aeroless.decoder

import scala.collection.generic.CanBuildFrom

import cats.Applicative
import io.aeroless.decoder.Decoder.DecoderAlgebra
import io.aeroless.{AsArray, AsLong, AsNull, AsObject, AsString, AsValue}

trait Decoder[A] {
  def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A]
}

object Decoder {

  trait DecoderAlgebra[F[_]] extends Applicative[F] {
    def field[A](field: String)(next: Decoder[A]): F[A]

    def at[A](idx: Int)(next: Decoder[A]): F[A]

    def opt[A](next: Decoder[A]): F[Option[A]]

    def readString: F[String]

    def readLong: F[Long]

    def readNull: F[Unit]

    def readValues[A, L[_]](next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): F[L[A]]

    def readFields[A](next: Decoder[A]): F[Map[String, A]]
  }

  object logs { self =>
    type Stack[A] = String

    private val interpreter = new DecoderAlgebra[Stack] {
      override def field[A](field: String)(next: Decoder[A]) = s"$field / " + log(next)

      override def at[A](idx: Int)(next: Decoder[A]) = s"[$idx]" + log(next)

      override def opt[A](next: Decoder[A]) = "?" + log(next)

      override def readString = "StringValue"

      override def readLong = "LongValue"

      override def readNull = "NullValue"

      override def readValues[A, L[_]](next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]) = " // " + log(next)

      override def readFields[A](next: Decoder[A]) = " /// " + log(next)

      override def pure[A](x: A) = ""

      override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]) = ff + "\n" + fa
    }

    def log[A](dsl: Decoder[A]): String = dsl.apply(interpreter)
  }

  object bins { self =>
    type Stack[A] = List[String]

    private val interpreter = new DecoderAlgebra[Stack] {
      override def field[A](field: String)(next: Decoder[A]): Stack[A] = List(field)

      override def at[A](idx: Int)(next: Decoder[A]): Stack[A] =  List("value")

      override def opt[A](next: Decoder[A]): Stack[A] = getBins(next)

      override def readString: Stack[String] = List("value")

      override def readLong: Stack[Long] = List("value")

      override def readNull: Stack[Unit] = Nil

      override def readValues[A, L[_]](next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Stack[L[A]] = List("value")

      override def readFields[A](next: Decoder[A]): Stack[Map[String, A]] = Nil //read all

      override def pure[A](x: A): Stack[A] = Nil

      override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]): Stack[B] = ff ++ fa
    }

    def getBins[A](prog: Decoder[A]): Stack[A] = prog(interpreter)
  }

  object yolo {

    import cats.instances.function._

    type Stack[A] = AsValue => A

    private val interpreter = new DecoderAlgebra[Stack] {
      override def field[A](field: String)(next: Decoder[A]): Stack[A] = {
        case o@AsObject(map) => o.get(field) match {
          case Some(f) => yolo.runUnsafe(next)(f)
          case None => throw ParseException(s"$field not present on object with fields ${map.keys.mkString(",")}.")
        }
        case e => throw ParseException(s"Try to parse object but $e is not.")
      }

      override def at[A](idx: Int)(next: Decoder[A]): Stack[A] = {
        case arr@AsArray(_) => arr.at(idx) match {
          case Some(v) => yolo.runUnsafe(next)(v)
          case None => throw ParseException(s"No value in array on index $idx.")
        }
        case e => throw ParseException(s"Try to parse array but $e is not.")
      }

      override def opt[A](next: Decoder[A]): Stack[Option[A]] = {
        case AsNull => None
        case v: AsValue => Some(yolo.runUnsafe(next).apply(v))
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
          arr.foreach(e => builder += yolo.runUnsafe(next).apply(e.value))
          builder.result()
        }
        case e => throw ParseException(s"Try read an array but $e is not.")
      }

      override def readFields[A](next: Decoder[A]): Stack[Map[String, A]] = {
        case AsObject(map) => map.mapValues(e => yolo.runUnsafe(next).apply(e.value))
        case e => throw ParseException(s"Try read an object but $e is not.")
      }

      override def pure[A](x: A): Stack[A] = Applicative[Stack].pure(x)

      override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]): Stack[B] = Applicative[Stack].ap(ff)(fa)

    }

    def runUnsafe[A](prog: Decoder[A]): Stack[A] = prog(interpreter)
  }

  implicit val readerApplicative: Applicative[Decoder] = new Applicative[Decoder] {
    override def pure[A](x: A): Decoder[A] = new Decoder[A] {
      override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A] = ev.pure(x)
    }

    override def ap[A, B](ff: Decoder[(A) => B])(fa: Decoder[A]): Decoder[B] = new Decoder[B] {

      override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[B] = ev.ap(ff.apply[F])(fa.apply[F])
    }
  }

  def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev

  import cats.implicits._
  import shapeless._
  import shapeless.labelled._

  def field[A](path: String)(implicit next: Decoder[A]): Decoder[A] = new Decoder[A] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A] = ev.field(path)(next)
  }

  def at[A](idx: Int)(implicit next: Decoder[A]): Decoder[A] = new Decoder[A] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A] = ev.at(idx)(next)
  }

  implicit def decoderOpt[A](implicit next: Decoder[A]): Decoder[Option[A]] = new Decoder[Option[A]] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[Option[A]] = ev.opt(next)
  }

  implicit def decoderString: Decoder[String] = new Decoder[String] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[String] = ev.readString
  }

  implicit def decoderLong: Decoder[Long] = new Decoder[Long] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[Long] = ev.readLong
  }

  implicit def decoderInt: Decoder[Int] = decoderLong.map(_.toInt)

  implicit def decoderBoolean: Decoder[Boolean] = decoderLong.map(_ > 0)

  implicit def decoderNull: Decoder[Unit] = new Decoder[Unit] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[Unit] = ev.readNull
  }

  implicit def decoderTraversable[A, L[_]](implicit next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Decoder[L[A]] = new Decoder[L[A]] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[L[A]] = ev.readValues(next, cbf)
  }

  implicit def decoderMap[A](implicit next: Decoder[A]): Decoder[Map[String, A]] = new Decoder[Map[String, A]] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[Map[String, A]] = ev.readFields(next)
  }

  implicit val decoderHNil: Decoder[HNil] = decoderNull.map(_ => HNil)

  implicit def decoderHList[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[H]],
    tDecoder: Lazy[Decoder[T]]
  ): Decoder[FieldType[K, H] :: T] = {
    import cats.implicits._
    (
      field[H](witness.value.name)(hDecoder.value),
      tDecoder.value
    ).mapN(shapeless.labelled.field[K](_) :: _)
  }

  implicit def decoderObject[A, Repr <: HList](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    hlistDecoder: Decoder[Repr]
  ): Decoder[A] = hlistDecoder.map(gen.from)
}
