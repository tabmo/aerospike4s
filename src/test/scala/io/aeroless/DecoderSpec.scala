package io.aeroless

import org.scalatest.{FlatSpec, Matchers}

class DecoderSpec extends FlatSpec with Matchers {

  import io.aeroless.parser._

  "Decoder" should "dsl.runEither long value" in {
    Decoder[Long].dsl.runEither(AsLong(1L)) shouldBe Right(1L)
  }

  "Decoder" should "dsl.runEither string value" in {
    Decoder[String].dsl.runEither(AsString("foo")) shouldBe Right("foo")
  }

  "Decoder" should "dsl.runEither option value" in {
    Decoder[Option[String]].dsl.runEither(AsString("foo")) shouldBe Right(Some("foo"))
    Decoder[Option[String]].dsl.runEither(AsNull) shouldBe Right(None)
  }

  "Decoder" should "dsl.runEither map value" in {
    val listValues = AsValue.obj(
      "value1" -> 1,
      "value2" -> 2L
    )

    Decoder[Map[String, Long]].dsl.runEither(listValues) shouldBe Right(Map("value1" -> 1L, "value2" -> 2L))
  }

  "Decoder" should "dsl.runEither list value" in {
    Decoder[List[String]].dsl.runEither(AsValue.arr("foo", "bar")) shouldBe Right(List("foo", "bar"))
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

    Decoder[Person].dsl.runEither(personAs) shouldBe Right(Person("Romain", 27, Contact("Rue de Thor"), Some(List("toto", "fifou"))))
  }

  "Decoder" should "be map" in {
    val booleanDecoder = Decoder[Long].map(l => if (l > 0) true else false)
    booleanDecoder.dsl.runEither(AsLong(1L)) shouldBe Right(true)
    booleanDecoder.dsl.runEither(AsLong(0L)) shouldBe Right(false)
  }
}
