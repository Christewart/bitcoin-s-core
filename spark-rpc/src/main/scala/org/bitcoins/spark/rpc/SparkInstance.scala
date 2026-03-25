package org.bitcoins.spark.rpc
import java.net.URI

sealed trait SparkInstance {
  def rpcUri: URI
}

/** A Spark operator instance for local / development use.
  *
  * @param rpcUri
  *   URI of the operator, e.g. {@code https://localhost:8535}
  * @param trustSelfSigned
  *   When true the gRPC client will accept self-signed TLS certificates. Only
  *   use this for local development — never in production.
  */
case class SparkInstanceLocal(
    rpcUri: URI,
    trustSelfSigned: Boolean = false
) extends SparkInstance
