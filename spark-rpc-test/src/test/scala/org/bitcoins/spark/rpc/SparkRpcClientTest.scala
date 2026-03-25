package org.bitcoins.spark.rpc

import com.google.protobuf.ByteString
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
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
    val instance =
      SparkInstanceLocal(
        new java.net.URI("https://localhost:8535"),
        trustSelfSigned = true
      )
    val client = SparkRpcClient(instance)

    // Identity key is used for challenge-response auth with the operator.
    // Signing key is the secp256k1 key associated with the deposit address.
    val identityKey = ECPrivateKey.freshPrivateKey
    val signingKey = ECPublicKey.freshPublicKey

    val req = GenerateDepositAddressRequest(
      signingPublicKey = signingKey.bytes,
      identityPublicKey = identityKey.publicKey.bytes
    )
    for {
      _ <- client.login(identityKey)
      info <- client.generateDepositAddress(req)
    } yield {
      println(s"Generated address: $info")
      succeed
    }
  }
}
