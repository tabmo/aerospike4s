package io.aerospike4s

import com.aerospike.client.Value
import com.aerospike.client.query.{Filter, Statement}

import io.aerospike4s.decoder.Decoder
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy}

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

  def aggregate[I](af: AggregateFunction[I])(inputValues: I)(implicit ev: I => FunctionValues[I]) = new QueryStatement[T] {
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
  implicit val longFunValues: Long => FunctionValues[Long] = (v: Long) => FunctionValues(Value.get(v) :: Nil)

  implicit val intFunValues: Int => FunctionValues[Int] = (v: Int) => FunctionValues(Value.get(v) :: Nil)

  implicit val stringFunValues: String => FunctionValues[String] = (v: String) => FunctionValues(Value.get(v) :: Nil)

  implicit val booleanFunValues: Boolean => FunctionValues[Boolean] = (v: Boolean) => FunctionValues(Value.get(v) :: Nil)

  implicit val floatFunValues: Float => FunctionValues[Float] = (v: Float) => FunctionValues(Value.get(v) :: Nil)

  implicit val doubleFunValues: Double => FunctionValues[Double] = (v: Double) => FunctionValues(Value.get(v) :: Nil)

  implicit val hnilFunValues: FunctionValues[HNil] = FunctionValues(Nil)

  implicit def hlistFunValues[H, T <: shapeless.HList](
    hEncoder: Lazy[H => FunctionValues[H]],
    tEncoder: Lazy[T => FunctionValues[T]]
  ): H :: T => FunctionValues[H :: T] = (hlist: (H :: T)) => {
    val hV = hEncoder.value(hlist.head).values
    val tV = tEncoder.value(hlist.tail).values
    FunctionValues(hV ++ tV)
  }

  def apply[T, Repr <: HList](obj: T)(implicit gen: LabelledGeneric.Aux[T, Repr], fv: Repr => FunctionValues[Repr]): FunctionValues[T] = FunctionValues(fv.apply(gen.to(obj)).values)
}
