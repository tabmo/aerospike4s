package io.aeroless.parser

import scala.collection.JavaConverters._

import com.aerospike.client.Value._
import com.aerospike.client.{Record, Value}

import cats.Eval

sealed trait AsValue { self =>

  /*
    Not efficient, use only for testing purpose
   */
  def asAerospikeValue: Value = {
    import collection.JavaConverters._
    self match {
      case AsNull => NullValue.INSTANCE
      case AsString(s) => new StringValue(s)
      case AsLong(l) => new LongValue(l)
      case AsObject(map) => {
        new MapValue(map.mapValues(_.value.asAerospikeValue).asJava)
      }
      case AsArray(arr) => {
        new ListValue(arr.map(_.value.asAerospikeValue).toList.asJava)
      }
    }
  }
}

object AsValue {

  def fromRecord(record: Record): AsValue = {
    def cast(value: AnyRef): AsValue = value match {
      case null => AsNull
      case s: String => AsString(s)
      case l: java.lang.Long => AsLong(l)
      case _: java.util.Map[_, _] => {
        val map = value.asInstanceOf[java.util.Map[String, AnyRef]]
          .asScala
          .mapValues { anyRef => Eval.later(cast(anyRef)) }
          .toMap
        AsObject(map)
      }
      case _: java.util.List[_] => {
        val array = value.asInstanceOf[java.util.List[AnyRef]]
          .toArray
          .map(anyRef => Eval.later(cast(anyRef)))
        AsArray(array)
      }
    }

    cast(record.bins)
  }

  implicit def stringToAsString(v: String): AsString = AsString(v)

  implicit def longToAsLong(v: Long): AsLong = AsLong(v)

  implicit def intToAsLong(v: Int): AsLong = AsLong(v.toLong)

  def obj(fields: (String, AsValue)*): AsObject = AsObject(fields.toMap.mapValues(Eval.now))

  def arr(fields: AsValue*): AsArray = AsArray(fields.toArray.map(Eval.now))
}

case object AsNull extends AsValue

case class AsObject(value: Map[String, Eval[AsValue]]) extends AsValue {

  def get(field: String): Option[AsValue] = value.get(field).map(_.value)

  def ++(obj: AsObject) = AsObject(value ++ obj.value)
}

case class AsArray(value: Array[Eval[AsValue]]) extends AsValue {

  def at(idx: Int): Option[AsValue] = if (idx >= value.length || idx < 0) None else Some(value(idx).value)
}

case class AsLong(value: Long) extends AsValue

case class AsString(value: String) extends AsValue
