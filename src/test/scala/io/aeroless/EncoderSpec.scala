package io.aeroless

import org.scalatest.{FlatSpec, Matchers}

class EncoderSpec extends FlatSpec with Matchers {

  import parser._
  import Encoder._

  "Encoder" should "encode long value" in {
    Encoder[Long].encode(1L) shouldBe AsLong(1L).asAerospikeValue
  }

  "Encoder" should "encode string value" in {
    Encoder[String].encode("foo") shouldBe AsString("foo").asAerospikeValue
  }

  "Encoder" should "encode option value" in {
    Encoder[Option[String]].encode(Some("foo")) shouldBe AsString("foo").asAerospikeValue
    Encoder[Option[String]].encode(None) shouldBe AsNull.asAerospikeValue
  }

  "AsDecoder" should "decode map value" in {
    val map = Map(
      "value1" -> 1L,
      "value2" -> 2L
    )

    Encoder[Map[String, Long]].encode(map) shouldBe AsValue.obj(
      "value1" -> 1L,
      "value2" -> 2L
    ).asAerospikeValue
  }

  "Encoder" should "encode list value" in {
    Encoder[List[String]].encode(List("foo")) shouldBe AsValue.arr("foo").asAerospikeValue
  }

  "Encoder" should "encode object value" in {

    case class Contact(address: String)
    case class Person(name: String, age: Long, contact: Contact, friends: Option[List[String]])

    val encodedObject = Encoder[Person].encode(Person("Romain", 27, Contact("Rue de Thor"), Some(List("toto", "fifou"))))

    encodedObject shouldBe AsValue.obj(
      "name" -> "Romain",
      "age" -> 27,
      "contact" -> AsValue.obj("address" -> "Rue de Thor"),
      "friends" -> AsValue.arr("toto", "fifou")
    ).asAerospikeValue
  }

  "Encoder" should "be contramap" in {
    val booleanEncoder = Encoder[Long].contramap[Boolean](b => if (b) 1L else 0L)
    booleanEncoder.encode(true) shouldBe AsLong(1L).asAerospikeValue
    booleanEncoder.encode(false) shouldBe AsLong(0L).asAerospikeValue
  }

}