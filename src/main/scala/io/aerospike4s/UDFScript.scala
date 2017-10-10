package io.aerospike4s

abstract class UDFScript(path: String, pack: String) { outer =>

  def function[T](funName: String): AggregateFunction[T] = {
    AggregateFunction[T](
      path = outer.path,
      pack = outer.pack,
      funName = funName
    )
  }
}
