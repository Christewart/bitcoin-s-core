package org.bitcoins.spark.rpc

import com.google.protobuf.ByteString
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstanceLocal
import org.bitcoins.spark.rpc.proto.spark.*
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime
import scodec.bits.ByteVector

import java.nio.file.Paths
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
    logger.info(s"Identity pubkey: ${identityKey.publicKey.hex}")
    logger.info(s"Signing pubkey: ${signingKey.hex}")
    val network = Network.REGTEST
    val path = if (EnvUtil.isMac) {
      Paths.get("/Users/chrisstewart/dev/spark/bitcoin_regtest.conf")
    } else {
      Paths.get("/home/chris/dev/spark/bitcoin_regtest.conf")
    }
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
      _ = logger.info(s"deposit address: ${address.address}")
      _ = logger.info(
        s"static deposit addresses: ${depositAddresses.depositAddresses.map(_.depositAddress)}")
      _ = assert(
        depositAddresses.depositAddresses.exists(
          _.depositAddress == address.address))

      depositTxId <- bitcoind.sendToAddress(
        BitcoinAddress.fromString(address.address),
        amt,
        walletName = "default")
      depositTx <- bitcoind.getRawTransactionRaw(depositTxId)
      vout = depositTx.outputs.zipWithIndex.find(_._1.value == amt).get._2
      _ = logger.info(s"Deposit txid=$depositTxId")
      _ <- bitcoind.generate(6)
      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      depositTreeCreationReq = StartDepositTreeCreationRequest(
        identityPublicKey = identityKey.publicKey.bytes,
        onChainUtxo =
          Some(UTXO(rawTx = depositTx.bytes, vout = vout, network = network))
      )
      depositTreeCreation <- sparkClient.startDepositTreeCreation(
        depositTreeCreationReq)
      _ = logger.info(s"Deposit tree creation: ${depositTreeCreation}")
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
