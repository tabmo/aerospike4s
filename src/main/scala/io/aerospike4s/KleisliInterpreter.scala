package io.aerospike4s

import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.{Failure, Success}

import com.aerospike.client.listener.{WriteListener, _}
import com.aerospike.client.{AerospikeException, Key, Record}

import cats.data.Kleisli
import cats.effect.Async
import cats.{Applicative, ~>}
import io.aerospike4s.AerospikeIO.{Add, Append, Bind, CreateIndex, Delete, DropIndex, Exists, Fail, Get, GetAll, Header, Join, Operate, Prepend, Pure, Put, Query, RegisterUDF, RemoveUDF, ScanAll, Touch}

object KleisliInterpreter {
  module =>

  import cats.implicits._

  def apply[F[_]](implicit ec: ExecutionContext, asyncF: Async[F]): AerospikeIO ~> Kleisli[F, AerospikeManager, ?] = λ[AerospikeIO ~> Kleisli[F, AerospikeManager, ?]] {

    case Put(key, bins) => writeOp[F] { (m, listener) =>
      if (bins.isEmpty) m.client.put(m.eventLoops.next(), listener, m.writePolicy.orNull, key)
      else m.client.put(m.eventLoops.next(), listener, m.writePolicy.orNull, key, bins: _*)
    }

    case Append(key, bins) => writeOp[F] { (m, listener) =>
      if (bins.isEmpty) m.client.append(m.eventLoops.next(), listener, m.writePolicy.orNull, key)
      else m.client.append(m.eventLoops.next(), listener, m.writePolicy.orNull, key, bins: _*)
    }

    case Prepend(key, bins) => writeOp[F] { (m, listener) =>
      if (bins.isEmpty) m.client.prepend(m.eventLoops.next(), listener, m.writePolicy.orNull, key)
      else m.client.prepend(m.eventLoops.next(), listener, m.writePolicy.orNull, key, bins: _*)
    }

    case Add(key, bins) => writeOp[F] { (m, listener) =>
      if (bins.isEmpty) m.client.add(m.eventLoops.next(), listener, m.writePolicy.orNull, key)
      else m.client.add(m.eventLoops.next(), listener, m.writePolicy.orNull, key, bins: _*)
    }

    case Touch(key) => writeOp[F] { (m, listener) =>
      m.client.touch(m.eventLoops.next(), listener, m.writePolicy.orNull, key)
    }

    case Delete(key) => op[F, Unit] { (m, cb) =>
      m.client.delete(m.eventLoops.next(), new DeleteListener {
        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onSuccess(key: Key, existed: Boolean): Unit = cb(Right(()))

      }, m.writePolicy.orNull, key)
    }

    case Exists(key) => op[F, Boolean] { (m, cb) =>
      m.client.exists(m.eventLoops.next(), new ExistsListener {
        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onSuccess(key: Key, exists: Boolean): Unit = cb(Right(exists))
      }, m.batchPolicy.orNull, key)
    }

    case Get(key, bins) => op[F, Option[AsValue]] { (m, cb) =>
      val listener = new RecordListener {
        override def onFailure(exception: AerospikeException) = cb(Left(exception))

        override def onSuccess(key: Key, record: Record) = cb(Right(Option(record).map(AsValue.fromRecord)))
      }

      if (bins.isEmpty) m.client.get(m.eventLoops.next(), listener, m.policy.orNull, key)
      else m.client.get(m.eventLoops.next(), listener, m.policy.orNull, key, bins: _*)
    }

    case Query(statement) => op[F, Vector[(Key, AsValue)]] { (m, cb) =>
      m.client.query(m.eventLoops.next(), new RecordSequenceListener {

        val results = Vector.newBuilder[(Key, AsValue)]

        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onRecord(key: Key, record: Record): Unit = {
          if (record.bins.containsKey("FAILURE")) {
            cb(Left(new AerospikeException(-99, record.bins.get("FAILURE").toString)))
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

        override def onSuccess(): Unit = cb(Right(results.result()))

      }, m.queryPolicy.orNull, statement.toStatement)
    }

    case ScanAll(ns, set, bins) => op[F, Vector[(Key, AsValue)]] { (m, cb) =>
      val listener = new RecordSequenceListener {
        val results = Vector.newBuilder[(Key, AsValue)]

        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onRecord(key: Key, record: Record): Unit = results += (key -> AsValue.fromRecord(record))

        override def onSuccess(): Unit = cb(Right(results.result()))
      }

      if (bins.isEmpty) m.client.scanAll(m.eventLoops.next(), listener, m.scanPolicy.orNull, ns, set)
      else m.client.scanAll(m.eventLoops.next(), listener, m.scanPolicy.orNull, ns, set, bins: _*)
    }

    case GetAll(keys, bins) => op[F, Vector[(Key, AsValue)]] { (m, cb) =>
      val listener = new RecordSequenceListener {
        val results = Vector.newBuilder[(Key, AsValue)]

        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onRecord(key: Key, record: Record): Unit = results += (key -> AsValue.fromRecord(record))

        override def onSuccess(): Unit = cb(Right(results.result()))
      }

      if (bins.isEmpty) m.client.get(m.eventLoops.next(), listener, m.batchPolicy.orNull, keys.toArray)
      else m.client.get(m.eventLoops.next(), listener, m.batchPolicy.orNull, keys.toArray, bins: _*)
    }

    //TODO not sure about what header do?
    case Header(key) => op[F, Unit] { (m, cb) =>
      m.client.getHeader(m.eventLoops.next(), new RecordListener {
        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onSuccess(key: Key, record: Record): Unit = cb(Right(()))
      }, m.policy.orNull, key)
    }

    case CreateIndex(ns, set, bin, idxType, idxOpt) => op[F, String] { (m, cb) =>
      Future {
        import scala.concurrent._
        blocking {
          val idx = idxOpt.getOrElse(s"${ns}_${set}_$bin")
          val task = m.client.createIndex(m.policy.orNull, ns, set, idx, bin, idxType)
          task.waitTillComplete()
          idx
        }
      }.onComplete {
        case Success(idx) => cb(Right(idx))
        case Failure(ex) => cb(Left(ex))
      }
    }

    case DropIndex(ns, set, idx) => op[F, Unit] { (m, cb) =>
      Future {
        blocking {
          m.client.dropIndex(m.policy.orNull, ns, set, idx)
        }
      }.onComplete {
        case Success(_) => cb(Right(()))
        case Failure(ex) => cb(Left(ex))
      }
    }

    case Operate(key, ops) => op[F, Option[AsValue]] { (m, cb) =>
      m.client.operate(m.eventLoops.next(), new RecordListener {
        override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

        override def onSuccess(key: Key, record: Record): Unit = cb(Right(Option(record).map(AsValue.fromRecord)))
      }, m.writePolicy.orNull, key, ops: _*)
    }

    case RegisterUDF(path, serverPath, loader, lang) => op[F, Unit] { (m, cb) =>
      Future {
        blocking {
          val t = m.client.register(m.policy.orNull, loader, path, serverPath, lang)
          t.waitTillComplete()
        }
      }.onComplete {
        case Success(_) => cb(Right(()))
        case Failure(ex) => cb(Left(ex))
      }
    }

    case RemoveUDF(serverPath) => op[F, Unit] { (m, cb) =>
      Future {
        blocking {
          m.client.removeUdf(m.infoPolicy.orNull, serverPath)
        }
      }.onComplete {
        case Success(_) => cb(Right(()))
        case Failure(ex) => cb(Left(ex))
      }
    }

    case Join(opA, opB) => {
      val a = module.apply[F].apply(opA)
      val b = module.apply[F].apply(opB)
      Applicative[Kleisli[F, AerospikeManager, ?]].product(a, b)
    }

    case Pure(x) => Kleisli.liftF(asyncF.pure(x))

    case Bind(x, f) => module.apply[F].apply(x).flatMap { a => module.apply[F].apply(f(a)) }

    case Fail(t) => Kleisli.liftF(asyncF.raiseError(t))
  }


  private def kleisli[F[_], A](f: AerospikeManager => F[A]): Kleisli[F, AerospikeManager, A] = Kleisli.apply[F, AerospikeManager, A](f)

  private def writeOp[F[_]](operation: (AerospikeManager, WriteListener) => Unit)(implicit asyncF: Async[F], ec: ExecutionContext): Kleisli[F, AerospikeManager, Unit] = op { (m, cb) =>
    val listener = new WriteListener {
      override def onFailure(exception: AerospikeException): Unit = cb(Left(exception))

      override def onSuccess(key: Key): Unit = cb(Right(()))
    }
    operation(m, listener)
  }

  private def op[F[_], A](operation: (AerospikeManager, Either[Throwable, A] => Unit) => Unit)(implicit asyncF: Async[F], ec: ExecutionContext): Kleisli[F, AerospikeManager, A] = kleisli { m =>
    asyncF.flatMap {
      asyncF.async[A] { cb =>
        operation(m, cb)
      }
    }(r => asyncF.shift(ec).map(_ => r))
  }
}