package org.bitcoins.spark.rpc

import com.google.protobuf.ByteString
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstanceLocal
import org.bitcoins.spark.rpc.proto.spark.{
  GenerateStaticDepositAddressRequest,
  Network,
  QueryBalanceRequest,
  QueryStaticDepositAddressesRequest
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

import java.nio.file.Paths
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class SparkRpcClientTest extends BitcoinSAsyncTest {
  behavior of "SparkRpcClient"
  implicit def byteVecToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toArray)

  implicit def byteStringToByteVec(byteString: ByteString): ByteVector =
    ByteVector(byteString.toByteArray)

  it must "generate a deposit address" in {
    val sparkInstance =
      SparkInstanceLocal(
        new java.net.URI("https://localhost:8535"),
        trustSelfSigned = true
      )
    val sparkClient = SparkRpcClient(sparkInstance)

    // Identity key is used for challenge-response auth with the operator.
    // Signing key is the secp256k1 key associated with the deposit address.
    val identityKey = ECPrivateKey.freshPrivateKey
    val signingKey = ECPublicKey.freshPublicKey
    val network = Network.REGTEST
    val path = Paths.get("/Users/chrisstewart/dev/spark/bitcoin_regtest.conf")
    val bitcoindInstance = BitcoindInstanceLocal.fromConfigFile(path.toFile)
    val bitcoind = BitcoindRpcClient(bitcoindInstance)
    val req = GenerateStaticDepositAddressRequest(
      identityPublicKey = identityKey.publicKey.bytes,
      signingPublicKey = signingKey.bytes,
      network = network
    )
    val amt = Bitcoins.one
    for {
      _ <- sparkClient.login(identityKey)
      info <- sparkClient.generateStaticDepositAddress(req)
      address = info.getDepositAddress
      depositAddresses <- sparkClient.queryStaticDepositAddresses(
        QueryStaticDepositAddressesRequest(identityPublicKey =
                                             identityKey.publicKey.bytes,
                                           network = network))
      _ = println(s"deposit address: ${address.address}")
      _ = println(
        s"static deposit addresses: ${depositAddresses.depositAddresses.map(_.depositAddress)}")
      depositTxId <- bitcoind.sendToAddress(
        BitcoinAddress.fromString(address.address),
        amt,
        walletName = "default")
      _ = println(s"Deposit txid=$depositTxId")
      _ <- bitcoind.generate(6)
      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      balanceReq = QueryBalanceRequest(identityPublicKey =
                                         identityKey.publicKey.bytes,
                                       network = network)
      balanceResp <- sparkClient.queryBalance(balanceReq)
    } yield {
      assert(balanceResp.balance == amt.satoshis.toLong)
      println(s"Generated address: $info")
      succeed
    }
  }
}
