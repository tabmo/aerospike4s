package io.aeroless.decoder

import org.scalatest.{FlatSpec, Matchers}

import io.aeroless.{AsLong, AsNull, AsString, AsValue}

class DecoderSpec extends FlatSpec with Matchers {
  import cats.implicits._

  "Decoder" should "dsl.runEither long value" in {
    Decoder[Long].runEither(AsLong(1L)) shouldBe Right(1L)
  }

  "Decoder" should "dsl.runEither string value" in {
    Decoder[String].runEither(AsString("foo")) shouldBe Right("foo")
  }

  "Decoder" should "dsl.runEither option value" in {
    Decoder[Option[String]].runEither(AsString("foo")) shouldBe Right(Some("foo"))
    Decoder[Option[String]].runEither(AsNull) shouldBe Right(None)
  }

  "Decoder" should "dsl.runEither map value" in {
    val listValues = AsValue.obj(
      "value1" -> 1,
      "value2" -> 2L
    )

    Decoder[Map[String, Long]].runEither(listValues) shouldBe Right(Map("value1" -> 1L, "value2" -> 2L))
  }

  "Decoder" should "dsl.runEither list value" in {
    Decoder[List[String]].runEither(AsValue.arr("foo", "bar")) shouldBe Right(List("foo", "bar"))
  }

  "Decoder" should "dsl.runEither object value" in {
    case class Contact(address: String)
    case class Person(name: String, age: Long, contact: Contact, friends: Option[List[String]])

    val personAs = AsValue.obj(
      "name" -> "Romain",
      "age" -> 27,
      "contact" -> AsValue.obj("address" -> "Rue de Thor"),
      "friends" -> AsValue.arr("toto", "fifou")
    )

    Decoder[Person].runEither(personAs) shouldBe Right(Person("Romain", 27, Contact("Rue de Thor"), Some(List("toto", "fifou"))))
  }

  "Decoder" should "be map" in {
    val booleanDecoder = Decoder[Long].map(l => if (l > 0) true else false)
    booleanDecoder.runEither(AsLong(1L)) shouldBe Right(true)
    booleanDecoder.runEither(AsLong(0L)) shouldBe Right(false)
  }
}
