package io.aeroless

import scala.collection.generic.CanBuildFrom
import scala.util.Try

import com.aerospike.client.command.ParticleType
import com.aerospike.client.{Bin, Value}

import cats.Applicative
import io.aeroless.parser.algebra.AsAlgebra

package object parser {

  object algebra {

    sealed trait AsAlgebra[F[_]] extends Applicative[F] {
      def get[A](field: String)(next: Dsl[A]): F[A]

      def at[A](idx: Int)(next: Dsl[A]): F[A]

      def opt[A](next: Dsl[A]): F[Option[A]]

      def readString: F[String]

      def readLong: F[Long]

      def readNull: F[Unit]

      def readValues[A, L[_]](next: Dsl[A], cbf: CanBuildFrom[Nothing, A, L[A]]): F[L[A]]

      def readFields[A](next: Dsl[A]): F[Map[String, A]]
    }

  }

  object logs { self =>
    type Stack[A] = String

    private val interpreter = new AsAlgebra[Stack] {
      override def get[A](field: String)(next: Dsl[A]) = s"$field / " + log(next)

      override def at[A](idx: Int)(next: Dsl[A]) = s"[$idx]" + log(next)

      override def opt[A](next: Dsl[A]) = "?" + log(next)

      override def readString = "StringValue"

      override def readLong = "LongValue"

      override def readNull = "NullValue"

      override def readValues[A, L[_]](next: Dsl[A], cbf: CanBuildFrom[Nothing, A, L[A]]) = " // " + log(next)

      override def readFields[A](next: Dsl[A]) = " /// " + log(next)

      override def pure[A](x: A) = ""

      override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]) = ff + "\n" + fa
    }

    def log[A](dsl: Dsl[A]): String = dsl.apply(interpreter)
  }

  object bins { self =>
    type Stack[A] = List[String]

    private val interpreter = new AsAlgebra[Stack] {
      override def get[A](field: String)(next: Dsl[A]): Stack[A] = List(field)

      override def at[A](idx: Int)(next: Dsl[A]): Stack[A] =  List("value")

      override def opt[A](next: Dsl[A]): Stack[A] = getBins(next)

      override def readString: Stack[String] = List("value")

      override def readLong: Stack[Long] = List("value")

      override def readNull: Stack[Unit] = Nil

      override def readValues[A, L[_]](next: Dsl[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Stack[L[A]] = List("value")

      override def readFields[A](next: Dsl[A]): Stack[Map[String, A]] = Nil //read all

      override def pure[A](x: A): Stack[A] = Nil

      override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]): Stack[B] = ff ++ fa
    }

    def getBins[A](prog: Dsl[A]): Stack[A] = prog(interpreter)
  }

  object yolo {

    import cats.instances.function._

    type Stack[A] = AsValue => A

    private val interpreter = new AsAlgebra[Stack] {
      override def get[A](field: String)(next: Dsl[A]): Stack[A] = {
        case o@AsObject(map) => o.get(field) match {
          case Some(f) => yolo.runUnsafe(next)(f)
          case None => throw ParseException(s"$field not present on object with fields ${map.keys.mkString(",")}.")
        }
        case e => throw ParseException(s"Try to parse object but $e is not.")
      }

      override def at[A](idx: Int)(next: Dsl[A]): Stack[A] = {
        case arr@AsArray(_) => arr.at(idx) match {
          case Some(v) => yolo.runUnsafe(next)(v)
          case None => throw ParseException(s"No value in array on index $idx.")
        }
        case e => throw ParseException(s"Try to parse array but $e is not.")
      }

      override def opt[A](next: Dsl[A]): Stack[Option[A]] = {
        case AsNull => None
        case v: AsValue => Some(yolo.runUnsafe(next).apply(v))
      }

      override def readString: Stack[String] = {
        case AsString(s) => s
        case e => throw ParseException(s"Try to parse a string value but $e is not.")
      }

      override def readLong: Stack[Long] = {
        case AsLong(l) => l
        case e => throw ParseException(s"Try to parse a long value but $e is not.")
      }

      override def readNull: Stack[Unit] = _ => ()

      override def readValues[A, L[A]](next: Dsl[A], cbf: CanBuildFrom[Nothing, A, L[A]]): Stack[L[A]] = {
        case AsArray(arr) => {
          val builder = cbf.apply()
          arr.foreach(e => builder += yolo.runUnsafe(next).apply(e.value))
          builder.result()
        }
        case e => throw ParseException(s"Try read an array but $e is not.")
      }

      override def readFields[A](next: Dsl[A]): Stack[Map[String, A]] = {
        case AsObject(map) => map.mapValues(e => yolo.runUnsafe(next).apply(e.value))
        case e => throw ParseException(s"Try read an object but $e is not.")
      }

      override def pure[A](x: A): Stack[A] = Applicative[Stack].pure(x)

      override def ap[A, B](ff: Stack[(A) => B])(fa: Stack[A]): Stack[B] = Applicative[Stack].ap(ff)(fa)

    }

    def runUnsafe[A](prog: Dsl[A]): Stack[A] = prog(interpreter)
  }

  sealed trait Dsl[A] {
    def apply[F[_]](implicit ev: AsAlgebra[F]): F[A]
  }

  def get[A](path: String)(next: Dsl[A]): Dsl[A] = new Dsl[A] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[A] = ev.get(path)(next)
  }

  def at[A](idx: Int)(next: Dsl[A]): Dsl[A] = new Dsl[A] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[A] = ev.at(idx)(next)
  }

  def opt[A](next: Dsl[A]): Dsl[Option[A]] = new Dsl[Option[A]] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[Option[A]] = ev.opt(next)
  }

  def readString: Dsl[String] = new Dsl[String] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[String] = ev.readString
  }

  def readLong: Dsl[Long] = new Dsl[Long] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[Long] = ev.readLong
  }

  def readNull: Dsl[Unit] = new Dsl[Unit] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[Unit] = ev.readNull
  }

  def readValues[A, L[_]](next: Dsl[A])(implicit cbf: CanBuildFrom[Nothing, A, L[A]]): Dsl[L[A]] = new Dsl[L[A]] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[L[A]] = ev.readValues(next, cbf)
  }

  def readFields[A](next: Dsl[A]): Dsl[Map[String, A]] = new Dsl[Map[String, A]] {
    override def apply[F[_]](implicit ev: AsAlgebra[F]): F[Map[String, A]] = ev.readFields(next)
  }

  implicit val dslApplicative: Applicative[Dsl] = new Applicative[Dsl] {
    override def pure[A](x: A): Dsl[A] = new Dsl[A] {
      override def apply[F[_]](implicit ev: AsAlgebra[F]): F[A] = ev.pure(x)
    }

    override def ap[A, B](ff: Dsl[(A) => B])(fa: Dsl[A]): Dsl[B] = new Dsl[B] {

      override def apply[F[_]](implicit ev: AsAlgebra[F]): F[B] = ev.ap(ff.apply[F])(fa.apply[F])
    }
  }

  implicit class DslOps[A](dsl: Dsl[A]) {
    def runEither(value: AsValue): Either[Throwable, A] = Try {
      yolo.runUnsafe(dsl)(value)
    }.toEither

    def getBins: Seq[String] = bins.getBins(dsl)

    def log: String = logs.log(dsl)
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
