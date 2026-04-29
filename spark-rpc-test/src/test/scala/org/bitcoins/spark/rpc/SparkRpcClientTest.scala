package org.bitcoins.spark.rpc

import com.google.protobuf.ByteString
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.{
  DLCStatus,
  DisjointUnionContractInfo,
  SingleContractInfo
}
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptSignature,
  TaprootScriptPubKey
}
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.frost.FrostNoncePriv
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindInstanceLocal
import org.bitcoins.spark.rpc.proto.common.SigningCommitment
import org.bitcoins.spark.rpc.proto.frost.SigningRole.USER
import org.bitcoins.spark.rpc.proto.frost.{
  FrostSigningJob,
  KeyPackage,
  SignFrostRequest,
  SigningNonce
}
import org.bitcoins.spark.rpc.proto.spark.*
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkit.wallet.{
  DLCWalletUtil,
  DualDLCWalletTestCachedBitcoind
}
import org.scalatest.FutureOutcome
import scodec.bits.ByteVector

import java.nio.file.Paths
import java.util.UUID
import scala.annotation.nowarn
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.implicitConversions

class SparkRpcClientTest extends DualDLCWalletTestCachedBitcoind {
  behavior of "SparkRpcClient"
  val path = if (EnvUtil.isMac) {
    Paths.get("/Users/chrisstewart/dev/spark/bitcoin_regtest.conf")
  } else {
    Paths.get("/home/chris/dev/spark/bitcoin_regtest.conf")
  }
  val bitcoindInstance = BitcoindInstanceLocal.fromConfigFile(path.toFile)
  lazy val bitcoind = BitcoindRpcClient(bitcoindInstance)

  // +1 from the embedded spark userid
  // https://github.com/buildonspark/spark/blob/main/spark/testing/wallet/signing.go#L24
  val userId =
    "0000000000000000000000000000000000000000000000000000000000000063"

  val sparkInstance =
    SparkInstanceLocal(
      new java.net.URI("https://localhost:8535"),
      trustSelfSigned = true,
      new java.net.URI("http://localhost:9990")
    )
  lazy val sparkClient = SparkRpcClient(sparkInstance)

  implicit def byteVecToByteString(byteVector: ByteVector): ByteString =
    ByteString.copyFrom(byteVector.toArray)

  implicit def byteStringToByteVec(byteString: ByteString): ByteVector =
    ByteVector(byteString.toByteArray)

  private val network = Network.REGTEST
  type FixtureParam =
    (FundedDLCWallet, FundedDLCWallet, BitcoindRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualFundedDLCWallets(test, bitcoind)
  }

  it must "deposit into a spark entity" in { _ =>
    val (identityKey, leafId, userKeyPackage) = setupSparkTest()
    val amt = Bitcoins.one
    for {
      depositResponse <- fundSparkAddress(
        sparkClient,
        amt,
        identityKey,
        leafId.toString,
        userKeyPackage
      )
      _ = assert(
        depositResponse.rootNode.exists(_.value == amt.satoshis.toLong))
      balanceReq = QueryBalanceRequest(identityPublicKey =
                                         identityKey.publicKey.bytes,
                                       network = network)
      balanceResp <- sparkClient.queryBalance(balanceReq)
    } yield {
      assert(balanceResp.balance == amt.satoshis.toLong)
      succeed
    }
  }

  it must "deposit into a spark entity and then transfer to another spark user" in {
    _ =>
      val (identityKey, leafId, userKeyPackage) = setupSparkTest()
      val fundingAmt = Bitcoins.one
      @nowarn val receiverPrivKey = ECPrivateKey.freshPrivateKey
      @nowarn val newLeafPrivKey = ECPrivateKey.freshPrivateKey
      for {
        depositResponse0 <- fundSparkAddress(
          sparkClient,
          fundingAmt,
          identityKey,
          leafId.toString,
          userKeyPackage
        )
        _ = assert(
          depositResponse0.rootNode.exists(
            _.value == fundingAmt.satoshis.toLong))
      } yield {
        succeed
      }

  }
  it must "deposit into a spark entity with a settled DLC" in { params =>
    val walletA = params._1.wallet
    // val walletB = params._2.wallet

    val sparkInstance =
      SparkInstanceLocal(
        new java.net.URI("https://localhost:8535"),
        trustSelfSigned = true,
        new java.net.URI("http://localhost:9990")
      )
    val sparkClient = SparkRpcClient(sparkInstance)

    // Identity key is used for challenge-response auth with the operator.
    // Signing key is the secp256k1 key associated with the deposit address.
    val identityKey = ECPrivateKey.freshPrivateKey
    val signingKey = ECPrivateKey.freshPrivateKey
    val signingPubKey = signingKey.publicKey
    logger.info(s"Identity pubkey: ${identityKey.publicKey.hex}")
    logger.info(s"Signing pubkey: ${signingPubKey.hex}")

    val leafId = UUID.randomUUID().toString
    val req = GenerateDepositAddressRequest(
      identityPublicKey = identityKey.publicKey.bytes,
      signingPublicKey = signingPubKey.bytes,
      network = network,
      leafId = Some(leafId)
    )
    val unusedDepositReq =
      QueryUnusedDepositAddressesRequest(identityPublicKey =
                                           identityKey.publicKey.bytes,
                                         network = network)
    // +1 from the embedde spark userid
    // https://github.com/buildonspark/spark/blob/main/spark/testing/wallet/signing.go#L24
    val userId =
      "0000000000000000000000000000000000000000000000000000000000000063"
    val pubShares = Map(userId -> byteVecToByteString(signingPubKey.bytes))
    val userKeyPackage = KeyPackage(identifier = userId,
                                    secretShare = signingKey.bytes,
                                    publicShares = pubShares,
                                    publicKey = signingPubKey.bytes,
                                    minSigners = 1)
    val amt = DLCWalletUtil.total
    val getSigningCommitmentReq =
      GetSigningCommitmentsRequest(count = 3, nodeIdCount = 1)

    for {
      _ <- sparkClient.login(identityKey)
      info <- sparkClient.generateDepositAddress(req)
      address = info.getDepositAddress
      sparkDepositAddress = BitcoinAddress.fromString(address.address)
      _ <- DLCWalletUtil.initDLC(
        params._1,
        params._2,
        SingleContractInfo(amt.satoshis,
                           DLCWalletUtil.sampleContractOraclePair),
        payoutAddressAOpt = Some(sparkDepositAddress)
      )
      dlc <- walletA.listDLCs().map(_.head)
      fundingTxId = DLCStatus.getFundingTxId(dlc)
      _ = logger.info(
        s"Funded DLC with id ${dlc.dlcId}, txid=${fundingTxId} executing DLC...")
      fundingTx <- walletA.getDLCFundingTx(DLCStatus.getContractId(dlc).get)
      _ = fundingTx.outputs.foreach(o =>
        logger.info(s"Funding tx output=$o address=${BitcoinAddress
            .fromScriptPubKey(o.scriptPubKey, RegTest)}"))
      settlementTx <- executeDLC(walletA, initiatorWins = true)
      _ <- bitcoind.sendRawTransaction(settlementTx)
      _ = settlementTx.inputs.map(_.previousOutput).foreach { o =>
        logger.info(s"Settlement tx outpoint=${o.txId.hex}:${o.vout.toInt}")
      }
      _ = settlementTx.outputs.foreach { o =>
        logger.info(
          s"Settlement tx output=$o address=${BitcoinAddress.fromScriptPubKey(o.scriptPubKey, RegTest)}")
      }
      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      depositAddresses <- sparkClient.queryUnusedDepositAddresses(
        unusedDepositReq)
      _ = logger.info(s"deposit address: ${address.address}")
      _ = logger.info(
        s"static deposit addresses: ${depositAddresses.depositAddresses.map(_.depositAddress)}")
      _ = assert(
        depositAddresses.depositAddresses.exists(
          _.depositAddress == address.address))
      verifyKey = ECPublicKey.fromBytes(address.verifyingKey)

      _ <- bitcoind.generate(6)

      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      (depositTreeCreationReq, signingArtifacts) = buildDepositTreeCreationReq(
        identityKey,
        settlementTx,
        amt,
        signingKey)
      rootTx = signingArtifacts.head.tx
      cpfpRefundTx = signingArtifacts(1).tx
      directCpfpRefundTx = signingArtifacts(2).tx
      //      depositTreeCreation <- sparkClient.startDepositTreeCreation(
      //        depositTreeCreationReq)
      //      _ = logger.info(
      //        s"Starting deposit tree creation: ${depositTreeCreation.treeId}, beginning signing flow...")
      getSigningCommitmentResp <- sparkClient.getSigningCommitments(
        getSigningCommitmentReq)
      frostJobs = toFrostSigningJobs(
        jobs = signingArtifacts,
        verifyingKey = verifyKey,
        userKeyPackage = userKeyPackage,
        sparkEntityCommitmentsResp = getSigningCommitmentResp
      )
      frostReq = SignFrostRequest(signingJobs = frostJobs, role = USER)
      _ = logger.info(
        s"Attempting to sign frost with ${frostJobs.size} signing jobs")
      frostResponse <- sparkClient.signFrost(frostReq)
      _ = logger.info(
        s"Done signing ${frostJobs.size} ${frostResponse.results.keys}")
      resultByJobId = frostResponse.results

      rootSig = resultByJobId(frostJobs(0).jobId).signatureShare
      refundSig = resultByJobId(frostJobs(1).jobId).signatureShare
      directSig = resultByJobId(frostJobs(2).jobId).signatureShare

      rootTxSigningJob = toUserSignedTxSigningJob(leafId,
                                                  rootTx,
                                                  rootSig,
                                                  frostJobs(0))
      cpfpRefundTxSigningJob = toUserSignedTxSigningJob(leafId,
                                                        cpfpRefundTx,
                                                        refundSig,
                                                        frostJobs(1))
      directFromCpfpRefundTxSigningJob =
        toUserSignedTxSigningJob(leafId,
                                 directCpfpRefundTx,
                                 directSig,
                                 frostJobs(2))

      finalizeDepTreeCreateReq = buildFinalizeDepositTreeCreationReq(
        depositTreeCreationReq = depositTreeCreationReq,
        rootTxSigningJob = rootTxSigningJob,
        cpfpRefundTxSigningJob = cpfpRefundTxSigningJob,
        directFromCpfpRefundTxSigningJob = directFromCpfpRefundTxSigningJob
      )
      depositResponse <- sparkClient.finalizeDepositTreeCreation(
        finalizeDepTreeCreateReq)
      _ = assert(
        depositResponse.rootNode.exists(_.value == amt.satoshis.toLong))
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

  private def fundSparkAddress(
      sparkClient: SparkRpcClient,
      fundingAmount: CurrencyUnit,
      identityKey: ECPrivateKey,
      leafId: String,
      userKeyPackage: KeyPackage)
      : Future[FinalizeDepositTreeCreationResponse] = {
    logger.info(
      s"Funding deposit address with leafId $leafId, signing pubkey ${userKeyPackage.publicKey.toHex}, fundingAmount=$fundingAmount")
    val getSigningCommitmentReq =
      GetSigningCommitmentsRequest(count = 3, nodeIdCount = 1)
    val signingKey = ECPrivateKey(userKeyPackage.secretShare)
    val signingPubKey = userKeyPackage.publicKey

    val req = GenerateDepositAddressRequest(
      identityPublicKey = identityKey.publicKey.bytes,
      signingPublicKey = signingPubKey,
      network = network,
      leafId = Some(leafId)
    )
    for {
      _ <- sparkClient.login(identityKey)
      info <- sparkClient.generateDepositAddress(req)
      address = info.getDepositAddress
      sparkDepositAddress = BitcoinAddress.fromString(address.address)
      verifyKey = ECPublicKey.fromBytes(address.verifyingKey)
      depositTxId <- bitcoind.sendToAddress(address = sparkDepositAddress,
                                            fundingAmount,
                                            walletName = "default")
      depositTx <- bitcoind.getRawTransactionRaw(depositTxId)

      _ <- bitcoind.generate(6)
      _ <- AsyncUtil.nonBlockingSleep(5.seconds)
      (depositTreeCreationReq, signingArtifacts) = buildDepositTreeCreationReq(
        identityKey,
        depositTx,
        fundingAmount,
        signingKey)
      rootTx = signingArtifacts.head.tx
      cpfpRefundTx = signingArtifacts(1).tx
      directCpfpRefundTx = signingArtifacts(2).tx
      //      depositTreeCreation <- sparkClient.startDepositTreeCreation(
      //        depositTreeCreationReq)
      //      _ = logger.info(
      //        s"Starting deposit tree creation: ${depositTreeCreation.treeId}, beginning signing flow...")
      getSigningCommitmentResp <- sparkClient.getSigningCommitments(
        getSigningCommitmentReq)
      frostJobs = toFrostSigningJobs(
        jobs = signingArtifacts,
        verifyingKey = verifyKey,
        userKeyPackage = userKeyPackage,
        sparkEntityCommitmentsResp = getSigningCommitmentResp
      )
      frostReq = SignFrostRequest(signingJobs = frostJobs, role = USER)
      _ = logger.info(
        s"Attempting to sign frost with ${frostJobs.size} signing jobs")
      frostResponse <- sparkClient.signFrost(frostReq)
      _ = logger.info(
        s"Done signing ${frostJobs.size} ${frostResponse.results.keys}")
      resultByJobId = frostResponse.results

      rootSig = resultByJobId(frostJobs(0).jobId).signatureShare
      refundSig = resultByJobId(frostJobs(1).jobId).signatureShare
      directSig = resultByJobId(frostJobs(2).jobId).signatureShare

      rootTxSigningJob = toUserSignedTxSigningJob(leafId,
                                                  rootTx,
                                                  rootSig,
                                                  frostJobs(0))
      cpfpRefundTxSigningJob = toUserSignedTxSigningJob(leafId,
                                                        cpfpRefundTx,
                                                        refundSig,
                                                        frostJobs(1))
      directFromCpfpRefundTxSigningJob =
        toUserSignedTxSigningJob(leafId,
                                 directCpfpRefundTx,
                                 directSig,
                                 frostJobs(2))

      finalizeDepTreeCreateReq = buildFinalizeDepositTreeCreationReq(
        depositTreeCreationReq = depositTreeCreationReq,
        rootTxSigningJob = rootTxSigningJob,
        cpfpRefundTxSigningJob = cpfpRefundTxSigningJob,
        directFromCpfpRefundTxSigningJob = directFromCpfpRefundTxSigningJob
      )
      depositResponse <- sparkClient.finalizeDepositTreeCreation(
        finalizeDepTreeCreateReq)
    } yield depositResponse
  }

  private def buildDepositTreeCreationReq(
      identityKey: ECPrivateKey,
      depositTx: Transaction,
      fundingAmt: CurrencyUnit,
      signingKey: ECPrivateKey): (StartDepositTreeCreationRequest,
                                  Vector[PreparedTxSigningArtifacts]) = {
    val depositOutputIdx = depositTx.outputs.zipWithIndex
      .find(_._1.value == fundingAmt)
      .get
      ._2

    // from https://github.com/buildonspark/spark/blob/f812667632ce5fef6069e02500856c6349516a49/spark/common/bitcoin.go#L32
    val fee = SatoshisPerVirtualByte(Satoshis(5)) * 191
    val depositNonce = FrostNoncePriv.fresh()
    val cpfpRefundNonce = FrostNoncePriv.fresh()
    val directFromCpfpRefundNonce = FrostNoncePriv.fresh()
    val utxoOpt = Some(
      UTXO(rawTx = depositTx.bytes, vout = depositOutputIdx, network = network))
    val depositSigningCommitment = SigningCommitment(
      hiding = depositNonce.k1.publicKey.bytes,
      binding = depositNonce.k2.publicKey.bytes
    )
    val cpfpRefundSigningCommitment = SigningCommitment(
      hiding = cpfpRefundNonce.k1.publicKey.bytes,
      binding = cpfpRefundNonce.k2.publicKey.bytes
    )
    val directFromCpfpRefundSigningCommitment = SigningCommitment(
      hiding = directFromCpfpRefundNonce.k1.publicKey.bytes,
      binding = directFromCpfpRefundNonce.k2.publicKey.bytes
    )
    val rootTx = buildRootTx(depositTx, depositOutputIdx)
    val rootTxOutputIdx = rootTx.outputs.zipWithIndex
      .find(_._1.value == fundingAmt)
      .get
      ._2
    logger.info(
      s"Deposit tx outpoint: ${depositTx.txIdBE.hex}:${depositOutputIdx}")
    logger.info(
      s"Root tx outpoint: ${rootTx.txIdBE.hex}:${rootTxOutputIdx} output count=${rootTx.outputs.size}")
    val rootTxSigningJobOpt = Some(
      SigningJob(
        signingPublicKey = signingKey.publicKey.bytes,
        rawTx = rootTx.bytes,
        signingNonceCommitment = Some(depositSigningCommitment)
      ))
    val refundSPK =
      TaprootScriptPubKey.fromInternalKey(signingKey.toXOnly)
    val cpfpRefundTx =
      buildCpfpRefundTx(rootTx = rootTx,
                        vout = rootTxOutputIdx,
                        refundSPK = refundSPK)
    val cpfpRefundTxSigningJobOpt = Some(
      SigningJob(
        signingPublicKey = signingKey.publicKey.bytes,
        rawTx = cpfpRefundTx.bytes,
        signingNonceCommitment = Some(cpfpRefundSigningCommitment)
      ))
    val directFromCpfpRefundTx =
      buildDirectFromCpfpRefundTx(rootTx = rootTx,
                                  vout = rootTxOutputIdx,
                                  refundSPK = refundSPK,
                                  fee = fee)
    val directFromCpfpRefundTxSigningJobOpt = Some(
      SigningJob(
        signingPublicKey = signingKey.publicKey.bytes,
        rawTx = directFromCpfpRefundTx.bytes,
        signingNonceCommitment = Some(directFromCpfpRefundSigningCommitment)
      )
    )
    val depositTreeCreationReq = StartDepositTreeCreationRequest(
      identityPublicKey = identityKey.publicKey.bytes,
      onChainUtxo = utxoOpt,
      rootTxSigningJob = rootTxSigningJobOpt,
      refundTxSigningJob = cpfpRefundTxSigningJobOpt,
      directFromCpfpRefundTxSigningJob = directFromCpfpRefundTxSigningJobOpt
    )

    val signingArtifacts = Vector(
      PreparedTxSigningArtifacts(
        rawTx = rootTx.bytes,
        fundingTx = depositTx,
        voutIdx = depositOutputIdx,
        nonce = depositNonce,
        job = rootTxSigningJobOpt.get
      ),
      PreparedTxSigningArtifacts(
        rawTx = cpfpRefundTx.bytes,
        fundingTx = rootTx,
        voutIdx = rootTxOutputIdx,
        nonce = cpfpRefundNonce,
        job = cpfpRefundTxSigningJobOpt.get
      ),
      PreparedTxSigningArtifacts(
        rawTx = directFromCpfpRefundTx.bytes,
        fundingTx = rootTx,
        voutIdx = rootTxOutputIdx,
        nonce = directFromCpfpRefundNonce,
        job = directFromCpfpRefundTxSigningJobOpt.get
      )
    )
    (depositTreeCreationReq, signingArtifacts)
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
    val wtx = WitnessTransaction(version = Int32(3),
                                 inputs = inputs,
                                 outputs = outputs,
                                 lockTime = TransactionConstants.lockTime,
                                 witness)
    wtx
  }
  private def buildCpfpRefundTx(
      rootTx: Transaction,
      vout: Int,
      refundSPK: ScriptPubKey): WitnessTransaction = {
    val fundingOutput = rootTx.outputs(vout).value
    val outpoint = TransactionOutPoint(rootTx.txIdBE, vout)
    logger.info(
      s"Building cpfp refund tx with outpoint ${outpoint.txId.hex}:${outpoint.vout}")
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
    val outpoint = TransactionOutPoint(rootTx.txIdBE, vout)
    logger.info(
      s"Building direct from cpfp refund tx with outpoint ${outpoint.txId.hex}:${outpoint.vout}")
    val inputs = Vector(
      TransactionInput(outpoint, ScriptSignature.empty, sequence))
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

  private def toFrostSigningJobs(
      jobs: Vector[PreparedTxSigningArtifacts],
      verifyingKey: ECPublicKey,
      userKeyPackage: KeyPackage,
      sparkEntityCommitmentsResp: GetSigningCommitmentsResponse)
      : Vector[FrostSigningJob] = {
    jobs.zipWithIndex.map { case (signingArtifact, idx) =>
      val jobId = java.util.UUID.randomUUID().toString
      val j = signingArtifact.job
      logger.info(
        s"Converting signing job ${j.getClass.getSimpleName} to frost signing job with id $jobId")
      FrostSigningJob(
        jobId = jobId,
        message = signingArtifact.sighash.bytes,
        keyPackage = Some(userKeyPackage),
        verifyingKey = verifyingKey.bytes,
        nonce = Some(
          SigningNonce(
            hiding = signingArtifact.nonce.k1.bytes,
            binding = signingArtifact.nonce.k2.bytes
          )),
        userCommitments = j.signingNonceCommitment,
        commitments = sparkEntityCommitmentsResp
          .signingCommitments(idx)
          .signingNonceCommitments
      )
    }
  }

  private def buildFinalizeDepositTreeCreationReq(
      depositTreeCreationReq: StartDepositTreeCreationRequest,
      rootTxSigningJob: UserSignedTxSigningJob,
      cpfpRefundTxSigningJob: UserSignedTxSigningJob,
      directFromCpfpRefundTxSigningJob: UserSignedTxSigningJob)
      : FinalizeDepositTreeCreationRequest = {
    FinalizeDepositTreeCreationRequest(
      identityPublicKey = depositTreeCreationReq.identityPublicKey,
      onChainUtxo = depositTreeCreationReq.onChainUtxo,
      rootTxSigningJob = Some(rootTxSigningJob),
      refundTxSigningJob = Some(cpfpRefundTxSigningJob),
      directFromCpfpRefundTxSigningJob = Some(directFromCpfpRefundTxSigningJob)
    )
  }

  private def toUserSignedTxSigningJob(
      leafId: String,
      transaction: Transaction,
      userSignature: ByteVector,
      frostSigningJob: FrostSigningJob): UserSignedTxSigningJob = {
    UserSignedTxSigningJob(
      leafId = leafId,
      signingPublicKey = frostSigningJob.keyPackage.map(_.publicKey).get,
      rawTx = transaction.bytes,
      signingNonceCommitment = frostSigningJob.userCommitments,
      userSignature = userSignature,
      signingCommitments = Some(SigningCommitments(frostSigningJob.commitments))
    )
  }

  private def setupSparkTest(): (ECPrivateKey, UUID, KeyPackage) = {
    // Identity key is used for challenge-response auth with the operator.
    // Signing key is the secp256k1 key associated with the deposit address.
    val identityKey = ECPrivateKey.freshPrivateKey
    val signingKey = ECPrivateKey.freshPrivateKey
    val signingPubKey = signingKey.publicKey
    logger.info(s"Identity pubkey: ${identityKey.publicKey.hex}")
    logger.info(s"Signing pubkey: ${signingPubKey.hex}")

    val leafId = UUID.randomUUID()

    val pubShares = Map(userId -> byteVecToByteString(signingPubKey.bytes))
    val userKeyPackage = KeyPackage(identifier = userId,
                                    secretShare = signingKey.bytes,
                                    publicShares = pubShares,
                                    publicKey = signingPubKey.bytes,
                                    minSigners = 1)
    (identityKey, leafId, userKeyPackage)
  }
  private def executeDLC(
      wallet: DLCWallet,
      initiatorWins: Boolean): Future[Transaction] = {
    for {
      contractId <- DLCWalletUtil.getContractId(wallet)
      status <- DLCWalletUtil.getDLCStatus(wallet)
      (initiatorSigs, receipientSigs) = status.contractInfo match {
        case single: SingleContractInfo =>
          DLCWalletUtil.getSigs(single)
        case disjoint: DisjointUnionContractInfo =>
          sys.error(
            s"Cannot retrieve sigs for disjoint union contract, got=$disjoint"
          )
      }
      settlementTx <- {
        if (initiatorWins) {
          wallet.executeDLC(contractId, initiatorSigs).map(_.get)
        } else {
          wallet.executeDLC(contractId, receipientSigs).map(_.get)
        }
      }
    } yield settlementTx
  }

}
