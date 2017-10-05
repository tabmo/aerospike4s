package io.aeroless

import com.aerospike.client.Value
import com.aerospike.client.query.{Filter, Statement}

import io.aeroless.parser.Decoder

class QueryStatementBuilder[T](
  namespace: String,
  set: String,
  decoder: Decoder[T]
) { outer =>

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
      s.setFilter(Filter.equal(bin, value))
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
    val bins = decoder.dsl.getBins
    if (bins.nonEmpty) s.setBinNames(bins: _*)
    s
  }
}

sealed trait QueryStatement[T] { outer =>
  val decoder: Decoder[T]

  def toStatement: Statement

  def aggregate(af: AggregateFunction)(values: Value*) = new QueryStatement[T] {
    val decoder: Decoder[T] = outer.decoder

    override def toStatement: Statement = {
      val s = outer.toStatement
      s.setAggregateFunction(af.classLoader, af.path, af.pack, af.funName, values: _*)
      s
    }
  }
}

case class AggregateFunction(path: String, pack: String, funName: String, classLoader: ClassLoader = DefaultClassLoader)
