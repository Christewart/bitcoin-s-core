package org.bitcoins.spark.rpc
import java.net.URI

sealed trait SparkInstance {

  def rpcUri: URI
}

case class SparkInstanceLocal(
    rpcUri: URI
) extends SparkInstance
