package io.aerospike4s.encoder

import com.aerospike.client.Value
import cats.{Contravariant, Semigroupal}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.IsHCons
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

trait Encoder[A] {
  def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[A]
}

object Encoder {

  import cats.implicits._

  def apply[A](implicit ev: Encoder[A]): Encoder[A] = ev

  implicit def opt[A](implicit next: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]) = {
      ev.opt(next)
    }
  }

  implicit def encoderString: Encoder[String] = new Encoder[String] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[String] = ev.writeString
  }

  implicit def encoderLong: Encoder[Long] = new Encoder[Long] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[Long] = ev.writeLong
  }

  implicit def encoderInt: Encoder[Int] = encoderLong.contramap(_.toLong)

  implicit def encoderBoolean: Encoder[Boolean] = encoderLong.contramap(b => if (b) 1L else 0L)

  implicit def encoderNull: Encoder[Unit] = new Encoder[Unit] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[Unit] = ev.writeNull
  }

  implicit def encoderRawValue: Encoder[Value] = new Encoder[Value] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[Value] = ev.writeRawValue
  }

  implicit def encoderMap[A](implicit next: Encoder[A]): Encoder[Map[String, A]] = new Encoder[Map[String, A]] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[Map[String, A]] = ev.writeFields(next)
  }

  implicit def encoderDeriveFromTraversable[A, L[A] <: Traversable[A]](implicit ev: Encoder[A]): Encoder[L[A]] = {
    encoderTraversable[A].contramap[L[A]](identity)
  }

  implicit def encoderTraversable[A](implicit next: Encoder[A]): Encoder[Traversable[A]] = new Encoder[Traversable[A]] {
    override def apply[F[_]](implicit ev: EncoderAlgebra[F]): F[Traversable[A]] = ev.writeValues(next)
  }

  implicit def hnilDecoder: Encoder[HNil] = encoderNull.contramap(_ => HNil)

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](
                                                         implicit witness: Witness.Aux[K],
                                                         isHCons: IsHCons.Aux[H :: T, H, T],
                                                         hDecoder: Lazy[Encoder[H]],
                                                         tDecoder: Lazy[Encoder[T]]
                                                       ): Encoder[FieldType[K, H] :: T] = {
    (io.aerospike4s.encoder.field(witness.value.name)(hDecoder.value), tDecoder.value).tupled.contramap[FieldType[K, H] :: T] { obj =>
      (isHCons.head(obj), isHCons.tail(obj))
    }
  }

  implicit def objectDecoder[A, Repr <: HList](
                                                implicit gen: LabelledGeneric.Aux[A, Repr],
                                                hlistDecoder: Encoder[Repr]
                                              ): Encoder[A] = hlistDecoder.contramap(gen.to)

  implicit val writerFunctor: Contravariant[Encoder] with Semigroupal[Encoder] = new Contravariant[Encoder] with Semigroupal[Encoder] {
    override def contramap[A, B](fa: Encoder[A])(f: (B) => A) = new Encoder[B] {
      override def apply[F[_]](implicit ev: EncoderAlgebra[F]) = {
        ev.contramap(fa(ev))(f)
      }
    }

    override def product[A, B](fa: Encoder[A], fb: Encoder[B]) = new Encoder[(A, B)] {
      override def apply[F[_]](implicit ev: EncoderAlgebra[F]) = ev.product(fa(ev), fb(ev))
    }
  }
}