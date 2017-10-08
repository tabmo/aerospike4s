package io.aeroless.encoder

import java.util

import com.aerospike.client.Value
import com.aerospike.client.Value.{MapValue, _}

import cats.Cartesian
import cats.functor.Contravariant
import io.aeroless.encoder.Encoder.WriterAlgebra
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.IsHCons

trait Encoder[A] {
  def apply[F[_]](implicit ev: WriterAlgebra[F]): F[A]
}

object Encoder {
  import cats.implicits._

  trait WriterAlgebra[F[_]] extends Contravariant[F] with Cartesian[F] {
    def field[A](field: String)(implicit next: Encoder[A]): F[A]

    def opt[A](next: Encoder[A]): F[Option[A]]

    def writeString: F[String]

    def writeLong: F[Long]

    def writeNull: F[Unit]

    def writeValues[A](implicit next: Encoder[A]): F[Traversable[A]]

    def writeFields[A](implicit next: Encoder[A]): F[Map[String, A]]
  }

  object valueEncoder {
    self =>

    type Stack[A] = A => Value

    private val interpreter = new WriterAlgebra[Stack] {
      override def field[A](field: String)(implicit next: Encoder[A]): Stack[A] = v => {
        val map = java.util.Collections.singletonMap(field, self.runWriter(next)(v))
        new MapValue(map)
      }

      override def opt[A](next: Encoder[A]): Stack[Option[A]] = {
        case Some(a) => self.runWriter(next)(a)
        case None => NullValue.INSTANCE
      }

      override def writeString: Stack[String] = v => new StringValue(v)

      override def writeLong: Stack[Long] = v => new LongValue(v)

      override def writeNull: Stack[Unit] = _ => NullValue.INSTANCE

      override def writeValues[A](implicit next: Encoder[A]): Stack[Traversable[A]] = list => {
        val javaList = new util.ArrayList[Value](list.size)
        var idx = 0
        list.foreach { e =>
          javaList.add(idx, self.runWriter(next)(e))
          idx = idx + 1
        }
        new ListValue(javaList)
      }

      override def writeFields[A](implicit next: Encoder[A]): Stack[Map[String, A]] = kv => {
        val javaMap = new util.HashMap[String, AnyRef](kv.size)
        kv.foreach { case (k, v) =>
          javaMap.put(k, self.runWriter(next)(v))
        }
        new MapValue(javaMap)
      }

      override def product[A, B](fa: Stack[A], fb: Stack[B]): Stack[(A, B)] = tuple => {
        (fa(tuple._1), fb(tuple._2)) match {
          case (m1: MapValue, m2: MapValue) =>
            val map1 = m1.getObject.asInstanceOf[java.util.Map[String, AnyRef]]
            val map2 = m2.getObject.asInstanceOf[java.util.Map[String, AnyRef]]
            //dirty optim with mutable map
            val newMap = {
              if (map1.isInstanceOf[java.util.HashMap[_, _]]) {
                map1.putAll(map2)
                map1
              } else if (map2.isInstanceOf[java.util.HashMap[_, _]]) {
                map2.putAll(map1)
                map2
              } else {
                val newMap = new util.HashMap[String, AnyRef](2)
                newMap.putAll(map1)
                newMap.putAll(map2)
                newMap
              }
            }
            new MapValue(newMap)
          case (a, _: NullValue) => a
          case (_: NullValue, b) => b
          case (_, b) => b
        }
      }

      override def contramap[A, B](fa: Stack[A])(f: (B) => A): Stack[B] = b => {
        fa(f(b))
      }
    }

    def runWriter[A](prog: Encoder[A]): Stack[A] = prog(interpreter)
  }

  def apply[A](implicit ev: Encoder[A]): Encoder[A] = ev

  implicit val writerFunctor: Contravariant[Encoder] with Cartesian[Encoder] = new Contravariant[Encoder] with Cartesian[Encoder] {
    override def contramap[A, B](fa: Encoder[A])(f: (B) => A) = new Encoder[B] {
      override def apply[F[_]](implicit ev: WriterAlgebra[F]) = {
        ev.contramap(fa(ev))(f)
      }
    }

    override def product[A, B](fa: Encoder[A], fb: Encoder[B]) = new Encoder[(A, B)] {
      override def apply[F[_]](implicit ev: WriterAlgebra[F]) = ev.product(fa(ev), fb(ev))
    }
  }

  def field[A](path: String)(implicit next: Encoder[A]): Encoder[A] = new Encoder[A] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]): F[A] = ev.field(path)(next)
  }

  implicit def opt[A](implicit next: Encoder[A]): Encoder[Option[A]] = new Encoder[Option[A]] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]) = {
      ev.opt(next)
    }
  }

  implicit def encoderString: Encoder[String] = new Encoder[String] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]): F[String] = ev.writeString
  }

  implicit def encoderLong: Encoder[Long] = new Encoder[Long] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]): F[Long] = ev.writeLong
  }

  implicit def encoderInt: Encoder[Int] = encoderLong.contramap(_.toLong)

  implicit def encoderBoolean: Encoder[Boolean] = encoderLong.contramap(b => if (b) 1L else 0L)

  implicit def encoderNull: Encoder[Unit] = new Encoder[Unit] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]): F[Unit] = ev.writeNull
  }

  implicit def encoderMap[A](implicit next: Encoder[A]): Encoder[Map[String, A]] = new Encoder[Map[String, A]] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]): F[Map[String, A]] = ev.writeFields(next)
  }

  implicit def encoderDeriveFromTraversable[A, L[A] <: Traversable[A]](implicit ev: Encoder[A]): Encoder[L[A]] = {
    encoderTraversable[A].contramap[L[A]](identity)
  }

  implicit def encoderTraversable[A](implicit next: Encoder[A]): Encoder[Traversable[A]] = new Encoder[Traversable[A]] {
    override def apply[F[_]](implicit ev: WriterAlgebra[F]): F[Traversable[A]] = ev.writeValues(next)
  }

  implicit val hnilDecoder: Encoder[HNil] = encoderNull.contramap(_ => ())

  implicit def hlistDecoder[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    isHCons: IsHCons.Aux[H :: T, H, T],
    hDecoder: Lazy[Encoder[H]],
    tDecoder: Lazy[Encoder[T]]
  ): Encoder[FieldType[K, H] :: T] = {
    field(witness.value.name)(hDecoder.value).product(tDecoder.value).contramap[FieldType[K, H] :: T] { obj =>
      (isHCons.head(obj), isHCons.tail(obj))
    }
  }

  implicit def objectDecoder[A, Repr <: HList](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    hlistDecoder: Encoder[Repr]
  ): Encoder[A] = hlistDecoder.contramap(gen.to)
}