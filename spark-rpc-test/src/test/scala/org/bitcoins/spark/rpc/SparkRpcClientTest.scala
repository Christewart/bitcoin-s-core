package org.bitcoins.spark.rpc

import com.google.protobuf.ByteString
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.spark.rpc.proto.spark.GenerateDepositAddressRequest
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

import scala.language.implicitConversions

class SparkRpcClientTest extends BitcoinSAsyncTest {
  behavior of "SparkRpcClient"
  implicit def byteVecToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toArray)

  implicit def byteStringToByteVec(byteString: ByteString): ByteVector =
    ByteVector(byteString.toByteArray)
  it must "generate a deposit address" in {
    val instance = SparkInstanceLocal(new java.net.URI("http://localhost:8080"))
    val client = SparkRpcClient(instance)
    val signingKey = ECPublicKey.freshPublicKey
    val identityKey = ECPublicKey.freshPublicKey
    val req = GenerateDepositAddressRequest(
      signingPublicKey = signingKey.bytes,
      identityPublicKey = identityKey.bytes
    )
    for {
      info <- client.generateDepositAddress(req)
    } yield {
      println(s"Generated address: ${info}")
      succeed
    }
  }
}
