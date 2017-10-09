package io.aerospike4s.syntax

import com.aerospike.client.command.ParticleType
import com.aerospike.client.{Bin, Value}

import io.aerospike4s.encoder.{Encoder, EncoderValueInterpreter}

trait EncoderSyntax {

  implicit class EncoderOps[A](encoder: Encoder[A]) {
    def encode(value: A): Value = EncoderValueInterpreter.encode(encoder).apply(value)
  }

  implicit class ValueOps(value: Value) {
    def toBins: Seq[Bin] = {
      import scala.collection.JavaConverters._
      value.getType match {
        case ParticleType.MAP => {
          value.getObject.asInstanceOf[java.util.Map[String, AnyRef]].asScala.map { case (k, v) =>
            new Bin(k, v)
          }.toSeq
        }
        case _ => new Bin("value", value.getObject) :: Nil
      }
    }
  }
}
