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
  * @param frostSignerSocketPath
  *   Optional path to the FROST signing service Unix socket. If not provided,
  *   FROST operations will attempt to go through the main RPC endpoint.
  *   Typically: `/tmp/frost_0.sock`
  */
case class SparkInstanceLocal(
    rpcUri: URI,
    trustSelfSigned: Boolean = false,
    frostSignerSocketPath: Option[String] = None
) extends SparkInstance
