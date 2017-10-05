package io

import scala.concurrent.{ExecutionContext, Future}

import com.aerospike.client.query.IndexType
import com.aerospike.client._

import cats.data.Kleisli
import io.aeroless.parser.Decoder

package object aeroless {

  val DefaultClassLoader = getClass.getClassLoader

  object keydomain {

    trait KeyBuilder {
      def apply(idx: String): Key

      def apply(idx: Long): Key

      def apply(idx: Int): Key
    }

    def apply(namespace: String, set: String): KeyBuilder = new KeyBuilder {
      override def apply(idx: String): Key = new Key(namespace, set, idx)

      override def apply(idx: Long): Key = new Key(namespace, set, idx)

      override def apply(idx: Int): Key = new Key(namespace, set, idx)
    }
  }

  object ops {
    def prepend(binName: String, value: String): Operation = Operation.prepend(new Bin(binName, value))

    def append(binName: String, value: String): Operation = Operation.append(new Bin(binName, value))

    def put(binName: String, value: String): Operation = Operation.put(new Bin(binName, value))

    def put(binName: String, value: Long): Operation = Operation.put(new Bin(binName, value))

    def put(binName: String, value: Int): Operation = Operation.put(new Bin(binName, value))

    def add(binName: String, value: Long): Operation = Operation.add(new Bin(binName, value))

    def add(binName: String, value: Int): Operation = Operation.add(new Bin(binName, value))

    def get(binName: String): Operation = Operation.get(binName)

    def getAll: Operation = Operation.get()

    def touch: Operation = Operation.touch()
  }

  object statement {

    def apply[T](namespace: String, set: String)(implicit decoder: Decoder[T]): QueryStatementBuilder[T] = {
      new QueryStatementBuilder(
        namespace = namespace,
        set = set,
        decoder
      )
    }
  }

  object connection {

    import AerospikeIO._
    import parser._

    def pure[A](x: A): AerospikeIO[A] = Pure(x)

    def put[T](key: Key, obj: T)(implicit encoder: Encoder[T]): AerospikeIO[Unit] = Put(key, encoder.encode(obj).toBins)

    def append(key: Key, bins: Map[String, String]): AerospikeIO[Unit] = {
      Append(key, bins.map { case (k, v) => new Bin(k, v) }.toSeq)
    }

    def prepend(key: Key, bins: Map[String, String]): AerospikeIO[Unit] = {
      Prepend(key, bins.map { case (k, v) => new Bin(k, v) }.toSeq)
    }

    def delete(key: Key): AerospikeIO[Unit] = {
      Delete(key)
    }

    def add(key: Key, numBin: Seq[(String, Long)]): AerospikeIO[Unit] = {
      Add(key, numBin.map { case (k, v) => new Bin(k, v) })
    }

    def get[T](key: Key)(implicit decoder: Decoder[T]): AerospikeIO[Option[T]] = {
      Get(key, decoder.dsl.getBins).flatMap {
        case Some(r) =>
          decoder.dsl.runEither(r) match {
            case Right(v) => AerospikeIO.successful(Some(v))
            case Left(err) => AerospikeIO.failed(err)
          }
        case None => AerospikeIO.successful(None)
      }
    }

    def touch(key: Key): AerospikeIO[Unit] = {
      Touch(key)
    }

    def header(key: Key): AerospikeIO[Unit] = {
      Header(key)
    }

    def exists(key: Key): AerospikeIO[Boolean] = {
      Exists(key)
    }

    def query[T](statement: QueryStatement[T]): AerospikeIO[Vector[(Key, T)]] = {
      Query(statement).flatMap(v => decodeVector[T](v)(statement.decoder))
    }

    def scanAll[T](namespace: String, set: String)
      (implicit decoder: Decoder[T]): AerospikeIO[Vector[(Key, T)]] = {
      ScanAll(namespace, set, decoder.dsl.getBins).flatMap(decodeVector[T])
    }

    def getAll[T](keys: Seq[Key])(implicit decoder: Decoder[T]): AerospikeIO[Vector[(Key, T)]] = {
      GetAll(keys, decoder.dsl.getBins).flatMap(decodeVector[T])
    }

    def createIndex(namespace: String, set: String, binName: String, idxType: IndexType, idx: Option[String] = None): AerospikeIO[String] = {
      CreateIndex(namespace, set, binName, idxType, idx)
    }

    def dropIndex(namespace: String, set: String, index: String): AerospikeIO[Unit] = {
      DropIndex(namespace, set, index)
    }

    def operate[T](key: Key)(ops: Operation*)(implicit decoder: Decoder[T]): AerospikeIO[Option[T]] = {
      Operate(key, ops).flatMap {
        case Some(r) =>
          decoder.dsl.runEither(r) match {
            case Right(v) => AerospikeIO.successful(Some(v))
            case Left(err) => AerospikeIO.failed(err)
          }
        case None => AerospikeIO.successful(None)
      }
    }

    def registerUDF(resourcePath: String, serverPath: String, loader: ClassLoader = DefaultClassLoader, language: Language = Language.LUA): AerospikeIO[Unit] = {
      RegisterUDF(resourcePath, serverPath, loader = loader, language = language)
    }

    def removeUdf(serverPath: String): AerospikeIO[Unit] = {
      RemoveUDF(serverPath)
    }

    private def decodeVector[T](vector: Vector[(Key, AsValue)])
      (implicit decoder: Decoder[T]): AerospikeIO[Vector[(Key, T)]] = {
      import cats.instances.either._
      import cats.instances.vector._
      import cats.syntax.traverse._

      vector.traverse[Either[Throwable, ?], (Key, T)] { t =>
        decoder.dsl.runEither(t._2).map(r => (t._1, r))
      } match {
        case Right(vec) => AerospikeIO.successful(vec)
        case Left(t) => AerospikeIO.failed(t)
      }
    }
  }

  implicit class AerospikeIOOps[A](io: AerospikeIO[A]) {

    def runFuture(manager: AerospikeManager)(implicit ec: ExecutionContext): Future[A] = {
      import cats.implicits._
      val f = BaseInterpreter[Kleisli[Future, AerospikeManager, ?]](LogInterpreter[Kleisli[Future, AerospikeManager, ?]](KleisliInterpreter.apply(ec)).apply).apply(io).apply(manager)
      f.failed.map(_.printStackTrace)
      f
    }
  }

}