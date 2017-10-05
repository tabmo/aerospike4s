package io.aeroless

import com.aerospike.client.Value
import com.aerospike.client.async.{AsyncClient, AsyncClientPolicy, EventLoops, NettyEventLoops}
import com.aerospike.client.query.IndexType
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfterAll, FlatSpec, GivenWhenThen, Matchers}

import io.netty.channel.nio.NioEventLoopGroup

class OpsSpec extends FlatSpec with Matchers with BeforeAndAfterAll with GivenWhenThen with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global

  import org.scalatest.time._

  import connection._

  implicit val patience = PatienceConfig.apply(timeout = Span(10, Seconds), interval = Span(300, Millis))

  val aerospikeClient: AsyncClient = new AsyncClient(new AsyncClientPolicy(), "localhost", 3000)

  val manager = new AerospikeManager {
    override val eventLoops: EventLoops = new NettyEventLoops(new NioEventLoopGroup(1))

    override val client: AsyncClient = aerospikeClient
  }

  val kd = keydomain("test", "set")

  case class TestValue(id: String)

  case class LongValue(value: Long)

  "Append / Prepend" should "work" in {
    val key = kd("AppendOps")
    val io = for {
      _ <- put(key, TestValue("value"))
      _ <- append(key, Map(
        "id" -> "_with_suffix"
      ))
      _ <- prepend(key, Map(
        "id" -> "with_prefix_"
      ))
      v <- get[TestValue](key)
    } yield v

    whenReady(io.runFuture(manager)) { r =>
      r should equal(Some(TestValue("with_prefix_value_with_suffix")))
    }
  }

  "Add operation" should "work" in {
    val key = kd("AddOps")
    val io = for {
      _ <- put(key, LongValue(1L))
      _ <- add(key, Seq(("value" -> 2L)))
      v <- get[LongValue](key)
    } yield v

    whenReady(io.runFuture(manager)) { r =>
      r should equal(Some(LongValue(3L)))
    }
  }

  "Delete operation" should "work" in {
    val key = kd("AddOps")
    val io = for {
      _ <- put(key, TestValue("value"))
      _ <- delete(key)
      v <- get[TestValue](key)
    } yield v

    whenReady(io.runFuture(manager)) { r =>
      r should equal(None)
    }
  }

  "Touch operation" should "work" in {
    val key = kd("TouchOps")
    val io = for {
      _ <- put(key, TestValue("value"))
      _ <- touch(key)
    } yield ()

    whenReady(io.runFuture(manager)) { _ =>
      ()
    }
  }

  "Header operation" should "work" in {
    val key = kd("HeaderOps")
    val io = for {
      _ <- put(key, TestValue("value"))
      _ <- header(key)
    } yield ()

    whenReady(io.runFuture(manager)) { _ =>
      ()
    }
  }

  "Exists operation" should "work" in {
    val key = kd("TouchOps")
    val io = for {
      _ <- put(key, TestValue("value"))
      existsKey <- exists(key)
      notExistsKey <- exists(kd("notexists"))
    } yield (existsKey, notExistsKey)

    whenReady(io.runFuture(manager)) { r =>
      r should equal((true, false))
    }
  }

  "Query statement operation" should "work" in {
    val kd = keydomain("test", "setQueryOps")
    val io = for {
      _ <- createIndex("test", "setQueryOps", "id", IndexType.STRING)
      _ <- put(kd("test_stmt_1"), TestValue("stmt1"))
      _ <- put(kd("test_stmt_2"), TestValue("stmt2"))
      record <- query(statement[TestValue]("test", "setQueryOps").binEqualTo("id", "stmt1"))
    } yield record.map(_._2)


    whenReady(io.runFuture(manager)) { records =>
      records should contain theSameElementsAs Seq(
        TestValue("stmt1")
      )
    }
  }

  "scan all operation" should "work" in {
    val io = for {
      _ <- put(kd("test_scan_1"), TestValue("scan1"))
      _ <- put(kd("test_scan_2"), TestValue("scan2"))
      _ <- put(kd("test_scan_3"), TestValue("scan3"))
      record <- scanAll[TestValue]("test", "set")
    } yield record.map(_._2)


    whenReady(io.runFuture(manager)) { records =>
      records should contain allElementsOf Seq(
        TestValue("scan1"),
        TestValue("scan2"),
        TestValue("scan3")
      )
    }
  }

  "Get all operation" should "work" in {
    val key1 = kd("test_getall_1")
    val key2 = kd("test_getall_2")

    val io = for {
      _ <- put(key1, TestValue("getall1"))
      _ <- put(key2, TestValue("getall2"))
      record <- getAll[TestValue](Seq(key1, key2))
    } yield record.map(_._2)

    whenReady(io.runFuture(manager)) { records =>
      records should contain theSameElementsAs Seq(
        TestValue("getall1"),
        TestValue("getall2")
      )
    }
  }

  "Create / Drop index operation" should "work" in {
    val io = for {
      _ <- createIndex("test", "set", "id", IndexType.STRING)
      _ <- dropIndex("test", "set", "test_set_id")
    } yield ()

    whenReady(io.runFuture(manager)) { _ =>
      ()
    }
  }

  "Operate operation" should "work" in {
    val key = kd("Operate_Ops")
    val io = operate[TestValue](key)(
      ops.put("id", "value"),
      ops.append("id", "_with_suffix"),
      ops.prepend("id", "with_prefix_"),
      ops.getAll
    )

    whenReady(io.runFuture(manager)) { r =>
      r should equal(Some(TestValue("with_prefix_value_with_suffix")))
    }
  }

  case class Person(name: String, age: Int)

  "Aggregate operation" should "work" in {
    val key = keydomain("test", "setAggregateOps")("Aggregate_Ops")
    val aggregateFunction = AggregateFunction(
      path = "persons.lua",
      pack = "persons",
      funName = "filterByAge"
    )

    val io = for {
      _ <- registerUDF("persons.lua", "persons.lua")
      _ <- createIndex("test", "setAggregateOps", "age", IndexType.NUMERIC)
      _ <- put(key, Person("Romain", 28))
      _ <- put(key, Person("Bob", 33))
      r <- query(statement[Person]("test", "setAggregateOps").onRange("age", 10, 40).aggregate(aggregateFunction)(
        Value.get(30)
      ))
      _ <- removeUdf("persons.lua")
    } yield r.map(_._2)

    whenReady(io.runFuture(manager)) { r =>
      r should contain theSameElementsAs Seq(
        Person("Bob", 33)
      )
    }
  }
}
