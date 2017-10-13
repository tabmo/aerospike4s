package io.aerospike4s.encoder

import java.util

import com.aerospike.client.Value
import com.aerospike.client.Value._

object EncoderValueInterpreter { self =>

  type Stack[A] = A => Value

  private val interpreter = new EncoderAlgebra[Stack] {
    override def field[A](field: String)(implicit next: Encoder[A]): Stack[A] = v => {
      val map = java.util.Collections.singletonMap(field, self.encode(next)(v))
      new MapValue(map)
    }

    override def opt[A](next: Encoder[A]): Stack[Option[A]] = {
      case Some(a) => self.encode(next)(a)
      case None => NullValue.INSTANCE
    }

    override def writeString: Stack[String] = v => new StringValue(v)

    override def writeLong: Stack[Long] = v => new LongValue(v)

    override def writeNull: Stack[Unit] = _ => new MapValue(util.Collections.emptyMap[String, AnyRef]())

    override def writeRawValue: Stack[Value] = identity

    override def writeValues[A](implicit next: Encoder[A]): Stack[Traversable[A]] = list => {
      val javaList = new util.ArrayList[Value](list.size)
      var idx = 0
      list.foreach { e =>
        javaList.add(idx, self.encode(next)(e))
        idx = idx + 1
      }
      new ListValue(javaList)
    }

    override def writeFields[A](implicit next: Encoder[A]): Stack[Map[String, A]] = kv => {
      val javaMap = new util.HashMap[String, AnyRef](kv.size)
      kv.foreach { case (k, v) =>
        javaMap.put(k, self.encode(next)(v))
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
        case (_, b) => b
      }
    }

    override def contramap[A, B](fa: Stack[A])(f: (B) => A): Stack[B] = b => {
      fa(f(b))
    }
  }

  def encode[A](prog: Encoder[A]): Stack[A] = prog(interpreter)
}
