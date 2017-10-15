# aerospike4s

A functional Aerospike client for Scala.

## Getting Started

Add resolver and library in your sbt file :
```scala
resolvers += "Tabmo Bintray" at "https://dl.bintray.com/tabmo/maven"

libraryDependencies += "io.tabmo" %% "aerospike4s" % "0.1.0-M2"
```

```scala
import io.aerospike4s._, connection._, syntax._

case class Foo(value1: String, value2: Long)

val namespace = "ns"
val set = "set"
val key = keydomain(namespace, set)("mykey")

val program: AerospikeIO[Option[Foo]] = for { 
  _ <- put(key, Foo("baz", 3L))
  r <- get[Foo](key)
} yield r

import scala.concurrent.ExecutionContext.Implicits.global
val client = AerospikeManager("localhost", 3000)
val result = Await.result(program.runFuture(client), Duration.Inf)
println(result)
client.close()
```
## Overview

This library is inspired by others database access libraries like [Doobie](http://tpolecat.github.io/doobie/). The
purpose of Aerospike4s is to provide a functional way to build program and libraries using Aerospike. You can safely add, delete and fetch 
records from Aerospike without having to deal with NPE and other ugly design.

Under the hood this library use the [official Aerospike java client](https://github.com/aerospike/aerospike-client-java).

## Documentation

TODO

## License

Code is provided under the Apache 2.0 license available at http://opensource.org/licenses/Apache-2.0, as well as in the LICENSE file.