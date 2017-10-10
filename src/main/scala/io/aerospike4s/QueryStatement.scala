package io.aerospike4s

import com.aerospike.client.Value
import com.aerospike.client.query.{Filter, Statement}

import io.aerospike4s.decoder.Decoder
import shapeless.{::, Generic, HList, HNil, Lazy}

class QueryStatementBuilder[T](
  namespace: String,
  set: String,
  decoder: Decoder[T]
) {
  outer =>

  def onRange(bin: String, start: Long, end: Long): QueryStatement[T] = new QueryStatement[T] {
    val decoder: Decoder[T] = outer.decoder

    override def toStatement: Statement = {
      val s = baseStatement()
      s.setFilter(Filter.range(bin, start, end))
      s
    }
  }

  def binEqualTo(bin: String, value: Long): QueryStatement[T] = new QueryStatement[T] {
    val decoder: Decoder[T] = outer.decoder

    override def toStatement: Statement = {
      val s = baseStatement()
      s.setFilter(Filter.equal(bin, value))
      s
    }
  }

  def binEqualTo(bin: String, value: Int): QueryStatement[T] = new QueryStatement[T] {
    val decoder: Decoder[T] = outer.decoder

    override def toStatement: Statement = {
      val s = baseStatement()
      s.setFilter(Filter.equal(bin, value.toLong))
      s
    }
  }

  def binEqualTo(bin: String, value: String): QueryStatement[T] = new QueryStatement[T] {
    val decoder: Decoder[T] = outer.decoder

    override def toStatement: Statement = {
      val s = baseStatement()
      s.setFilter(Filter.equal(bin, value))
      s
    }
  }

  private def baseStatement(): Statement = {
    import syntax._
    val s = new Statement()
    s.setNamespace(namespace)
    s.setSetName(set)
    val bins = decoder.getBins
    if (bins.nonEmpty) s.setBinNames(bins: _*)
    s
  }
}

sealed trait QueryStatement[T] {
  outer =>
  val decoder: Decoder[T]

  def toStatement: Statement

  def aggregate[I](af: AggregateFunction[I])(inputValues: I)(implicit ev: FunctionValues.FV[I]) = new QueryStatement[T] {
    val decoder: Decoder[T] = outer.decoder

    override def toStatement: Statement = {
      val s = outer.toStatement
      s.setAggregateFunction(af.classLoader, af.path, af.pack, af.funName, ev(inputValues).values: _*)
      s
    }
  }
}

case class AggregateFunction[T](path: String, pack: String, funName: String, classLoader: ClassLoader = DefaultClassLoader)

case class FunctionValues[T](values: Seq[Value]) extends AnyVal

object FunctionValues {
  type FV[A] = A => FunctionValues[A]

  implicit val longFunValues: FV[Long] = (v: Long) => FunctionValues(Value.get(v) :: Nil)

  implicit val intFunValues: FV[Int] = (v: Int) => FunctionValues(Value.get(v) :: Nil)

  implicit val stringFunValues: FV[String] = (v: String) => FunctionValues(Value.get(v) :: Nil)

  implicit val booleanFunValues: FV[Boolean] = (v: Boolean) => FunctionValues(Value.get(v) :: Nil)

  implicit val floatFunValues: FV[Float] = (v: Float) => FunctionValues(Value.get(v) :: Nil)

  implicit val doubleFunValues: FV[Double] = (v: Double) => FunctionValues(Value.get(v) :: Nil)

  implicit val hnilFunValues: FV[HNil] = _ => FunctionValues(Nil)

  implicit def hlistFunValues[H, T <: shapeless.HList](
    implicit hEncoder: Lazy[FV[H]],
    tEncoder: Lazy[FV[T]]
  ): FV[H :: T] = (hlist: (H :: T)) => {
    val hV = hEncoder.value(hlist.head).values
    val tV = tEncoder.value(hlist.tail).values
    FunctionValues(hV ++ tV)
  }

  implicit def objectFunValues[T, Repr <: HList](implicit gen: Generic.Aux[T, Repr], fv: FV[Repr]): FV[T] = (obj: T) => {
    FunctionValues(fv.apply(gen.to(obj)).values)
  }
}
