package io.aerospike4s.decoder

import org.scalatest.{FlatSpec, Matchers}

class DslSpec extends FlatSpec with Matchers {
  import cats.implicits._
  import io.aerospike4s._, syntax._

  val aerospikeValue = AsValue.obj(
    "name" -> "Romain",
    "age" -> 27,
    "details" -> AsValue.obj(
      "city" -> "Montpellier",
      "company" -> "Tabmo"
    )
  )

  val program = {
    import Decoder._
    (
      field[String]("name"),
      field[Long]("age"),
      field("details") {
        field[String]("city")
      }
    ).tupled
  }


  "Value" should "be read" in {
    program.runEither(aerospikeValue) shouldBe Right(("Romain", 27, "Montpellier"))
  }

  case class Details(city: String, company: String)

  case class Person(name: String, age: Int, details: Option[Details])

  "Value" should "be decode" in {
    val tested = Decoder[Person].runEither(aerospikeValue)
    tested shouldBe Right(Person("Romain", 27, Some(Details("Montpellier", "Tabmo"))))
  }

  "Value" should "extract bins seq" in {
    val tested = program.getBins
    tested shouldBe Seq("name", "age", "details")
  }
}
