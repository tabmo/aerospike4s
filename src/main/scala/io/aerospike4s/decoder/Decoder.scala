package io.aerospike4s.decoder

import scala.collection.generic.CanBuildFrom

import cats.Applicative
import io.aerospike4s.AsValue

trait Decoder[A] {
  def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A]
}

object Decoder {

  import cats.implicits._
  import shapeless._
  import shapeless.labelled._

  def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev

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

  implicit def decoderRawValues: Decoder[AsValue] = new Decoder[AsValue] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[AsValue] = ev.readRawValue
  }

  implicit def decoderTraversable[A, L[_]](implicit next: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Decoder[L[A]] = new Decoder[L[A]] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[L[A]] = ev.readValues(next, cbf)
  }

  implicit def decoderMap[A](implicit next: Decoder[A]): Decoder[Map[String, A]] = new Decoder[Map[String, A]] {
    override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[Map[String, A]] = ev.readFields(next)
  }

  implicit def decoderHNil: Decoder[HNil] = decoderNull.map(_ => HNil)

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

  implicit val decoderApplicative: Applicative[Decoder] = new Applicative[Decoder] {
    override def pure[A](x: A): Decoder[A] = new Decoder[A] {
      override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[A] = ev.pure(x)
    }

    override def ap[A, B](ff: Decoder[(A) => B])(fa: Decoder[A]): Decoder[B] = new Decoder[B] {

      override def apply[F[_]](implicit ev: DecoderAlgebra[F]): F[B] = ev.ap(ff.apply[F])(fa.apply[F])
    }
  }
}
