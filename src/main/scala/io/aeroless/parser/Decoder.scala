package io.aeroless.parser

import scala.collection.generic.CanBuildFrom

case class Decoder[A](dsl: Dsl[A]) {

  def map[B](f: A => B): Decoder[B] = {
    import cats.syntax.functor._
    Decoder(dsl.map(f))
  }
}

object Decoder {

  implicit val decodeString: Decoder[String] = Decoder(readString)

  implicit val decodeUnit: Decoder[Unit] = Decoder(readNull)

  implicit val decodeLong: Decoder[Long] = Decoder(readLong)

  implicit val decodeInt: Decoder[Int] = decodeLong.map(_.toInt)

  implicit val decodeBoolean: Decoder[Boolean] = decodeLong.map(_ > 0)

  implicit def deriveDecodeTraversable[A, L[A] <: Traversable[A]](implicit ev: Decoder[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Decoder[L[A]] = {
    Decoder(readValues[A, L](ev.dsl))
  }

  implicit def decodeMap[A](implicit ev: Decoder[A]): Decoder[Map[String, A]] = Decoder(readFields(ev.dsl))

  implicit def decodeOption[A](implicit ev: Decoder[A]): Decoder[Option[A]] = Decoder(opt(ev.dsl))

  import shapeless._
  import shapeless.labelled._

  implicit val hnilDecoder: Decoder[HNil] = decodeUnit.map(_ => HNil)

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hDecoder: Lazy[Decoder[H]],
    tDecoder: Lazy[Decoder[T]]
  ): Decoder[FieldType[K, H] :: T] = Decoder {
    import cats.implicits._
    (
      get(witness.value.name)(hDecoder.value.dsl),
      tDecoder.value.dsl
    ).mapN(field[K](_) :: _)
  }

  implicit def objectDecoder[A, Repr <: HList](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    hlistDecoder: Decoder[Repr]
  ): Decoder[A] = hlistDecoder.map(gen.from)

  def apply[A](implicit ev: Decoder[A]): Decoder[A] = ev
}
