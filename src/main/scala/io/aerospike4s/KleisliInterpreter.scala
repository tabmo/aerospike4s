package io.aerospike4s

import scala.concurrent.{ExecutionContext, Future, Promise, blocking}

import com.aerospike.client.listener.{WriteListener, _}
import com.aerospike.client.{AerospikeException, Key, Record}

import cats.data.Kleisli
import cats.~>
import io.aerospike4s.AerospikeIO.{Add, Append, Bind, CreateIndex, Delete, DropIndex, Exists, Fail, Get, GetAll, Header, Join, Operate, Prepend, Pure, Put, Query, RegisterUDF, RemoveUDF, ScanAll, Touch}

object KleisliInterpreter { module =>

  import cats.implicits._

  def apply(implicit ec: ExecutionContext): AerospikeIO ~> Kleisli[Future, AerospikeManager, ?] = λ[AerospikeIO ~> Kleisli[Future, AerospikeManager, ?]] {

    case Put(key, bins) => kleisli[Unit] { m =>

      val promise = Promise[Unit]()

      m.client.put(m.eventLoops.next(), new WriteListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key): Unit = promise.success(())

      }, m.writePolicy.orNull, key, bins: _*)


      promise.future
    }

    case Append(key, bins) => kleisli[Unit] { m =>
      val promise = Promise[Unit]

      m.client.append(m.eventLoops.next(), new WriteListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key): Unit = promise.success(())

      }, m.writePolicy.orNull, key, bins: _*)

      promise.future
    }

    case Prepend(key, bins) => kleisli[Unit] { m =>
      val promise = Promise[Unit]

      m.client.prepend(m.eventLoops.next(), new WriteListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key): Unit = promise.success(())

      }, m.writePolicy.orNull, key, bins: _*)

      promise.future
    }

    case Add(key, bins) => kleisli[Unit] { m =>
      val promise = Promise[Unit]

      m.client.add(m.eventLoops.next(), new WriteListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key): Unit = promise.success(())

      }, m.writePolicy.orNull, key, bins: _*)

      promise.future
    }

    case Delete(key) => kleisli[Unit] { m =>
      val promise = Promise[Unit]

      m.client.delete(m.eventLoops.next(), new DeleteListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key, existed: Boolean): Unit = promise.success(())

      }, m.writePolicy.orNull, key)

      promise.future
    }

    case Touch(key) => kleisli[Unit] { m =>
      val promise = Promise[Unit]

      m.client.touch(m.eventLoops.next(), new WriteListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key): Unit = promise.success(())

      }, m.writePolicy.orNull, key)

      promise.future
    }

    case Exists(key) => kleisli[Boolean] { m =>
      val promise = Promise[Boolean]

      m.client.exists(m.eventLoops.next(), new ExistsListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key, exists: Boolean): Unit = promise.success(exists)
      }, m.batchPolicy.orNull, key)

      promise.future
    }

    case Get(key, bins) => kleisli[Option[AsValue]] { m =>
      val promise = Promise[Option[AsValue]]

      m.client.get(m.eventLoops.next(), new RecordListener {
        override def onFailure(exception: AerospikeException) = promise.failure(exception)

        override def onSuccess(key: Key, record: Record) = promise.success(Option(record).map(AsValue.fromRecord))
      }, m.policy.orNull, key, bins: _*)

      promise.future
    }

    case Query(statement) => kleisli[Vector[(Key, AsValue)]] { m =>
      val promise = Promise[Vector[(Key, AsValue)]]()

      m.client.query(m.eventLoops.next(), new RecordSequenceListener {

        val results = Vector.newBuilder[(Key, AsValue)]

        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onRecord(key: Key, record: Record): Unit = {
          if (record.bins.containsKey("FAILURE")) {
            promise.failure(new AerospikeException(-99, record.bins.get("FAILURE").toString))
          } else if (record.bins.containsKey("SUCCESS")) {
            val newRecord = new Record(
              record.getMap("SUCCESS").asInstanceOf[java.util.Map[String, AnyRef]],
              record.generation,
              record.expiration
            )
            results += (key -> AsValue.fromRecord(newRecord))
          } else {
            results += (key -> AsValue.fromRecord(record))
          }
        }

        override def onSuccess(): Unit = promise.success(results.result())

      }, m.queryPolicy.orNull, statement.toStatement)

      promise.future
    }

    case ScanAll(ns, set, bins) => kleisli[Vector[(Key, AsValue)]] { m =>
      val promise = Promise[Vector[(Key, AsValue)]]()

      m.client.scanAll(m.eventLoops.next(), new RecordSequenceListener {

        val results = Vector.newBuilder[(Key, AsValue)]

        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onRecord(key: Key, record: Record): Unit = results += (key -> AsValue.fromRecord(record))

        override def onSuccess(): Unit = promise.success(results.result())

      }, m.scanPolicy.orNull, ns, set, bins: _*)

      promise.future
    }

    case GetAll(keys, bins) => kleisli[Vector[(Key, AsValue)]] { m =>
      val promise = Promise[Vector[(Key, AsValue)]]()

      m.client.get(m.eventLoops.next(), new RecordSequenceListener {

        val results = Vector.newBuilder[(Key, AsValue)]

        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onRecord(key: Key, record: Record): Unit = results += (key -> AsValue.fromRecord(record))

        override def onSuccess(): Unit = promise.success(results.result())

      }, m.batchPolicy.orNull, keys.toArray, bins: _*)

      promise.future
    }

    //TODO not sure about what header do?
    case Header(key) => kleisli[Unit] { m =>
      val promise = Promise[Unit]

      m.client.getHeader(m.eventLoops.next(), new RecordListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key, record: Record): Unit = promise.success(())
      }, m.policy.orNull, key)

      promise.future
    }

    case CreateIndex(ns, set, bin, idxType, idxOpt) => kleisli[String] { m =>
      Future {
        import scala.concurrent._
        blocking {
          val idx = idxOpt.getOrElse(s"${ns}_${set}_$bin")
          val task = m.client.createIndex(m.policy.orNull, ns, set, idx, bin, idxType)
          task.waitTillComplete()
          idx
        }
      }
    }

    case DropIndex(ns, set, idx) => kleisli[Unit] { m =>
      Future {
        blocking {
          m.client.dropIndex(m.policy.orNull, ns, set, idx)
        }
      }
    }

    case Operate(key, ops) => kleisli[Option[AsValue]] { m =>
      val promise = Promise[Option[AsValue]]

      m.client.operate(m.eventLoops.next(), new RecordListener {
        override def onFailure(exception: AerospikeException): Unit = promise.failure(exception)

        override def onSuccess(key: Key, record: Record): Unit = promise.success(Option(record).map(AsValue.fromRecord))
      }, m.writePolicy.orNull, key, ops: _*)

      promise.future
    }

    case RegisterUDF(path, serverPath, loader, lang) => kleisli[Unit] { m =>
      Future {
        blocking {
          val t = m.client.register(m.policy.orNull, loader, path, serverPath, lang)
          t.waitTillComplete()
        }
      }
    }

    case RemoveUDF(serverPath) => kleisli[Unit] { m =>
      Future {
        blocking {
          m.client.removeUdf(m.infoPolicy.orNull, serverPath)
        }
      }
    }

    case Pure(x) => Kleisli.lift(Future.successful(x))

    case Join(opA, opB) =>
      kleisli { m =>
        val fa = module.apply(ec)(opA)(m)
        val fb = module.apply(ec)(opB)(m)
        for {
          a <- fa
          b <- fb
        } yield (a, b)
      }

    case Bind(x, f) =>
      module.apply(ec)(x).flatMap { a => module.apply(ec)(f(a)) }

    case Fail(t) => Kleisli.lift(Future.failed(t))
  }


  private def kleisli[A](f: AerospikeManager => Future[A]): Kleisli[Future, AerospikeManager, A] = Kleisli.apply[Future, AerospikeManager, A](f)
}