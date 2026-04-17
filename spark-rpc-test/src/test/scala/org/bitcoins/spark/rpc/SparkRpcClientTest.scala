package org.bitcoins.spark.rpc

import com.google.protobuf.ByteString
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptSignature,
  TaprootScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput,
  TransactionWitness,
  WitnessTransaction
}
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.frost.FrostNoncePriv
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstanceLocal
import org.bitcoins.spark.rpc.proto.common.SigningCommitment
import org.bitcoins.spark.rpc.proto.spark.*
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
  private val network = Network.REGTEST

  it must "deposit into a spark entity" in {
    val sparkInstance =
      SparkInstanceLocal(
        new java.net.URI("https://localhost:8535"),
        trustSelfSigned = true
      )
    val sparkClient = SparkRpcClient(sparkInstance)

    // Identity key is used for challenge-response auth with the operator.
    // Signing key is the secp256k1 key associated with the deposit address.
    val identityKey = ECPrivateKey.freshPrivateKey
    val signingKey = ECPrivateKey.freshPrivateKey
    val signingPubKey = signingKey.publicKey
    logger.info(s"Identity pubkey: ${identityKey.publicKey.hex}")
    logger.info(s"Signing pubkey: ${signingPubKey.hex}")

    val path = if (EnvUtil.isMac) {
      Paths.get("/Users/chrisstewart/dev/spark/bitcoin_regtest.conf")
    } else {
      Paths.get("/home/chris/dev/spark/bitcoin_regtest.conf")
    }
    val bitcoindInstance = BitcoindInstanceLocal.fromConfigFile(path.toFile)
    val bitcoind = BitcoindRpcClient(bitcoindInstance)
    val req = GenerateDepositAddressRequest(
      identityPublicKey = identityKey.publicKey.bytes,
      signingPublicKey = signingPubKey.bytes,
      network = network
    )
    val unusedDepositReq =
      QueryUnusedDepositAddressesRequest(identityPublicKey =
                                           identityKey.publicKey.bytes,
                                         network = network)

    val amt = Bitcoins.one
    for {
      _ <- sparkClient.login(identityKey)
      info <- sparkClient.generateDepositAddress(req)
      address = info.getDepositAddress
      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      depositAddresses <- sparkClient.queryUnusedDepositAddresses(
        unusedDepositReq)
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

      _ = logger.info(s"Deposit txid=$depositTxId")
      _ <- bitcoind.generate(6)
      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      depositTreeCreationReq = buildDepositTreeCreationReq(identityKey,
                                                           depositTx,
                                                           amt,
                                                           signingKey)
      depositTreeCreation <- sparkClient.startDepositTreeCreation(
        depositTreeCreationReq)
      _ = logger.info(
        s"Starting deposit tree creation: ${depositTreeCreation.treeId}")
      balanceReq = QueryBalanceRequest(identityPublicKey =
                                         identityKey.publicKey.bytes,
                                       network = network)
      _ <- AsyncUtil.nonBlockingSleep(5.second)
      balanceResp <- sparkClient.queryBalance(balanceReq)
    } yield {
      assert(balanceResp.balance == amt.satoshis.toLong)
      println(s"Generated address: $info")
      succeed
    }
  }

  private def buildDepositTreeCreationReq(
      identityKey: ECPrivateKey,
      depositTx: Transaction,
      fundingAmt: CurrencyUnit,
      signingKey: ECPrivateKey): StartDepositTreeCreationRequest = {
    val vout = depositTx.outputs.zipWithIndex
      .find(_._1.value == fundingAmt)
      .get
      ._2

    // from https://github.com/buildonspark/spark/blob/f812667632ce5fef6069e02500856c6349516a49/spark/common/bitcoin.go#L32
    val fee = SatoshisPerVirtualByte(Satoshis(5)) * 191
    val depositNonce = FrostNoncePriv.fresh()
    val cpfpRefundNonce = FrostNoncePriv.fresh()
    val utxoOpt = Some(
      UTXO(rawTx = depositTx.bytes, vout = vout, network = network))
    val depositSigningCommitment = SigningCommitment(
      hiding = depositNonce.k1.publicKey.bytes,
      binding = depositNonce.k2.publicKey.bytes
    )
    val cpfpRefundSigningCommitment = SigningCommitment(
      hiding = cpfpRefundNonce.k1.publicKey.bytes,
      binding = cpfpRefundNonce.k2.publicKey.bytes
    )
    val rootTx = buildRootTx(depositTx, vout)
    val rootTxSigningJobOpt = Some(
      SigningJob(
        signingPublicKey = signingKey.publicKey.bytes,
        rawTx = rootTx.bytes,
        signingNonceCommitment = Some(depositSigningCommitment)
      ))
    val refundSPK =
      TaprootScriptPubKey.fromInternalKey(signingKey.toXOnly)
    val cpfpRefundTx =
      buildCpfpRefundTx(rootTx = rootTx, vout = vout, refundSPK = refundSPK)
    val cpfpRefundTxSigningJobOpt = Some(
      SigningJob(
        signingPublicKey = signingKey.publicKey.bytes,
        rawTx = cpfpRefundTx.bytes,
        signingNonceCommitment = Some(cpfpRefundSigningCommitment)
      ))
    val directFromCpfpRefundTx =
      buildDirectFromCpfpRefundTx(rootTx = rootTx,
                                  vout = vout,
                                  refundSPK = refundSPK,
                                  fee = fee)
    val directFromCpfpRefundTxSigningJobOpt = Some(
      SigningJob(
        signingPublicKey = signingKey.publicKey.bytes,
        rawTx = directFromCpfpRefundTx.bytes,
        signingNonceCommitment = Some(cpfpRefundSigningCommitment)
      )
    )
    val depositTreeCreationReq = StartDepositTreeCreationRequest(
      identityPublicKey = identityKey.publicKey.bytes,
      onChainUtxo = utxoOpt,
      rootTxSigningJob = rootTxSigningJobOpt,
      refundTxSigningJob = cpfpRefundTxSigningJobOpt,
      directFromCpfpRefundTxSigningJob = directFromCpfpRefundTxSigningJobOpt
    )
    depositTreeCreationReq
  }

  private def buildRootTx(
      depositTx: Transaction,
      vout: Int): WitnessTransaction = {
    val fundingOutput = depositTx.outputs(vout)
    val outpoint = TransactionOutPoint(depositTx.txIdBE, vout)
    val inputs = Vector(
      TransactionInput(outpoint,
                       ScriptSignature.empty,
                       TransactionConstants.lockTime))
    val outputs = Vector(
      fundingOutput,
      TransactionOutput.ephemeralAnchor
    )
    val witness = TransactionWitness.fromWitOpt(Vector(None))
    WitnessTransaction(version = Int32(3),
                       inputs = inputs,
                       outputs = outputs,
                       lockTime = TransactionConstants.lockTime,
                       witness)
  }
  private def buildCpfpRefundTx(
      rootTx: Transaction,
      vout: Int,
      refundSPK: ScriptPubKey): WitnessTransaction = {
    val fundingOutput = rootTx.outputs(vout).value
    val outpoint = TransactionOutPoint(rootTx.txIdBE, vout)
    val refundSequence = UInt32(2000)
    val inputs = Vector(
      TransactionInput(outpoint, ScriptSignature.empty, refundSequence)
    )
    val outputs = Vector(TransactionOutput(value = fundingOutput, refundSPK),
                         TransactionOutput.ephemeralAnchor)
    val witness = TransactionWitness.fromWitOpt(Vector(None))
    WitnessTransaction(version = Int32(3),
                       inputs = inputs,
                       outputs = outputs,
                       lockTime = TransactionConstants.lockTime,
                       witness)
  }

  private def buildDirectFromCpfpRefundTx(
      rootTx: Transaction,
      vout: Int,
      refundSPK: ScriptPubKey,
      fee: CurrencyUnit): WitnessTransaction = {
    val fundingOutput = rootTx.outputs(vout)
    val sequence = UInt32(2000) + UInt32(50)
    val inputs = Vector(
      TransactionInput(TransactionOutPoint(rootTx.txIdBE, vout),
                       ScriptSignature.empty,
                       sequence))
    val outputs = Vector(
      TransactionOutput(value = fundingOutput.value - fee, refundSPK)
    )
    val refund = WitnessTransaction(
      version = Int32(3),
      inputs = inputs,
      outputs = outputs,
      lockTime = TransactionConstants.lockTime,
      witness = TransactionWitness.fromWitOpt(Vector(None))
    )
    refund
  }
}
