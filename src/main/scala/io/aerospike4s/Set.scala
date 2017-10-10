package io.aerospike4s

import io.aerospike4s.keydomain.KeyBuilder

case class Set(namespace: String, setName: String) {

  def key: KeyBuilder = keydomain.apply(namespace, setName)
}
