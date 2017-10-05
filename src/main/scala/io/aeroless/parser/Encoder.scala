package io.aeroless.parser

import java.util

import com.aerospike.client.Value
import com.aerospike.client.Value._

trait Encoder[A] {
  self =>

  def encode(a: A): Value

  def contramap[B](f: B => A): Encoder[B] = (b: B) => self.encode(f(b))
}

object Encoder {

  import shapeless._
  import shapeless.labelled._
  import shapeless.ops.hlist.IsHCons

  private def instance[A](f: A => Value) = new Encoder[A] {
    override def encode(a: A): Value = f(a)
  }

  implicit val longEncoder: Encoder[Long] = instance(v => new LongValue(v))

  implicit val intEncoder: Encoder[Int] = longEncoder.contramap(_.toLong)

  implicit val booleanEncoder: Encoder[Boolean] = longEncoder.contramap(b => if (b) 1L else 0L)

  implicit val stringEncoder: Encoder[String] = instance(v => new StringValue(v))

  implicit def mapEncoder[V](implicit evV: Encoder[V]): Encoder[Map[String, V]] = instance { kv =>
    val javaMap = new util.HashMap[String, AnyRef](kv.size)
    kv.foreach { case (k, v) =>
      javaMap.put(k, evV.encode(v))
    }
    new MapValue(javaMap)
  }

  implicit def deriveFromTraversableEncoder[A, L[A] <: Traversable[A]](implicit ev: Encoder[A]): Encoder[L[A]] = {
    traversableEncoder[A].contramap[L[A]](identity)
  }

  implicit def traversableEncoder[A](implicit ev: Encoder[A]): Encoder[Traversable[A]] = instance { list =>
    val javaList = new util.ArrayList[Value](list.size)
    var idx = 0
    list.foreach { e =>
      javaList.add(idx, ev.encode(e))
      idx = idx + 1
    }
    new ListValue(javaList)
  }

  implicit def optionEncoder[A](implicit ev: Encoder[A]): Encoder[Option[A]] = instance {
    case Some(a) => ev.encode(a)
    case None => NullValue.INSTANCE
  }

  implicit val hnilEncoder: Encoder[HNil] = instance(_ => new MapValue(new java.util.HashMap[String, AnyRef]()))

  implicit def hlistEncoder[K <: Symbol, H, T <: shapeless.HList](
    implicit witness: Witness.Aux[K],
    isHCons: IsHCons.Aux[H :: T, H, T],
    hEncoder: Lazy[Encoder[H]],
    tEncoder: Lazy[Encoder[T]]
  ): Encoder[FieldType[K, H] :: T] = instance { o =>

    /*
      dirty mutable code below. we keep reference to the same MapValue with a mutable HashMap inside.
      Recursively fill it with (field -> value) pair.
     */
    val headValue = hEncoder.value.encode(isHCons.head(o))
    val mapValue = tEncoder.value.encode(isHCons.tail(o))
    val map = mapValue.getObject.asInstanceOf[java.util.Map[String, AnyRef]]
    map.put(witness.value.name, headValue)
    mapValue
  }

  implicit def objectEncoder[A, Repr <: HList](
    implicit gen: LabelledGeneric.Aux[A, Repr],
    hlistEncoder: Encoder[Repr]
  ): Encoder[A] = (o: A) => {
    hlistEncoder.encode(gen.to(o))
  }

  def apply[A](implicit ev: Encoder[A]): Encoder[A] = ev
}



