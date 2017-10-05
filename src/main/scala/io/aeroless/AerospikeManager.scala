package io.aeroless

import com.aerospike.client.async.{AsyncClient, EventLoops}
import com.aerospike.client.policy._

trait AerospikeManager {
  val client: AsyncClient
  val eventLoops: EventLoops

  val policy: Option[Policy] = None
  val queryPolicy: Option[QueryPolicy] = None
  val writePolicy: Option[WritePolicy] = None
  val batchPolicy: Option[BatchPolicy] = None
  val infoPolicy: Option[InfoPolicy] = None
  //val adminPolicy: Option[AdminPolicy] = None
  //val generationPolicy: Option[GenerationPolicy] = None
  val scanPolicy: Option[ScanPolicy] = None
}
