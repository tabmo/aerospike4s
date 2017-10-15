package io.aerospike4s

import com.aerospike.client.AerospikeClient
import com.aerospike.client.async.{EventLoops, NioEventLoops}
import com.aerospike.client.policy._

trait AerospikeManager {
  val client: AerospikeClient
  val eventLoops: EventLoops

  val policy: Option[Policy] = None
  val queryPolicy: Option[QueryPolicy] = None
  val writePolicy: Option[WritePolicy] = None
  val batchPolicy: Option[BatchPolicy] = None
  val infoPolicy: Option[InfoPolicy] = None
  //val adminPolicy: Option[AdminPolicy] = None
  //val generationPolicy: Option[GenerationPolicy] = None
  val scanPolicy: Option[ScanPolicy] = None

  def close(): Unit = {
    client.close()
    eventLoops.close()
  }
}

object AerospikeManager {

  def apply(host: String, port: Int): AerospikeManager = {
    new AerospikeManager {
      override val eventLoops: EventLoops = new NioEventLoops(Runtime.getRuntime.availableProcessors())

      override val client: AerospikeClient = {
        import com.aerospike.client.policy.ClientPolicy
        val clientPolicy = new ClientPolicy
        clientPolicy.eventLoops = eventLoops
        new AerospikeClient(clientPolicy, host, port)
      }
    }
  }
}
