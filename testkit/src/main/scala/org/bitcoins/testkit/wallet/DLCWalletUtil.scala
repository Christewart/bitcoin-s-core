package org.bitcoins.testkit.wallet

import grizzled.slf4j.Logging
import org.bitcoins.core.crypto.WitnessTxSigComponent
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{BIP32Path, HDAccount}
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.dlc.testgen.DLCTestUtil
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.scalatest.Assertions.fail
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object DLCWalletUtil extends Logging {
  lazy val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  lazy val kValues: Vector[ECPrivateKey] =
    0.to(10).map(_ => ECPrivateKey.freshPrivateKey).toVector
  lazy val rValues: Vector[SchnorrNonce] = kValues.map(_.schnorrNonce)

  lazy val kValue: ECPrivateKey = kValues.head
  lazy val rValue: SchnorrNonce = rValues.head

  lazy val winStr: String = "WIN"
  lazy val loseStr: String = "LOSE"

  lazy val winHash: Sha256Digest =
    CryptoUtil.sha256DLCAttestation(winStr)

  lazy val loseHash: Sha256Digest =
    CryptoUtil.sha256DLCAttestation(loseStr)

  val sampleOutcomes: Vector[(EnumOutcome, Satoshis)] = Vector(
    EnumOutcome(winStr) -> Satoshis(10000),
    EnumOutcome(loseStr) -> Satoshis.zero)

  lazy val sampleContractDescriptor: EnumContractDescriptor =
    EnumContractDescriptor(sampleOutcomes)

  lazy val sampleOracleInfo: EnumSingleOracleInfo =
    EnumSingleOracleInfo.dummyForKeys(oraclePrivKey,
                                      rValue,
                                      sampleOutcomes.map(_._1))

  lazy val sampleContractOraclePair =
    ContractOraclePair.EnumPair(sampleContractDescriptor, sampleOracleInfo)

  lazy val sampleContractInfo: ContractInfo =
    ContractInfo(Satoshis(10000), sampleContractOraclePair)

  lazy val sampleOracleWinSig: SchnorrDigitalSignature =
    oraclePrivKey.schnorrSignWithNonce(winHash.bytes, kValue)

  lazy val sampleOracleLoseSig: SchnorrDigitalSignature =
    oraclePrivKey.schnorrSignWithNonce(loseHash.bytes, kValue)

  val numDigits: Int = 6

  lazy val multiNonceContractDescriptor: NumericContractDescriptor =
    DLCTestUtil.genMultiDigitContractInfo(numDigits, Satoshis(10000))._1

  lazy val multiNonceOracleInfo: NumericSingleOracleInfo =
    NumericSingleOracleInfo(
      OracleAnnouncementV0TLV.dummyForKeys(oraclePrivKey,
                                           rValues.take(numDigits)))

  lazy val multiNonceContractOraclePair = {
    ContractOraclePair.NumericPair(multiNonceContractDescriptor,
                                   multiNonceOracleInfo)
  }

  lazy val multiNonceContractInfo: ContractInfo =
    ContractInfo(Satoshis(10000), multiNonceContractOraclePair)

  lazy val dummyContractMaturity: BlockTimeStamp = BlockTimeStamp(1666335)
  lazy val dummyContractTimeout: BlockTimeStamp = BlockTimeStamp(1666337)

  lazy val dummyTimeouts: DLCTimeouts =
    DLCTimeouts(dummyContractMaturity, dummyContractTimeout)

  lazy val dummyKey: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyKey2: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyPartialSig: PartialSignature =
    PartialSignature(dummyKey, DummyECDigitalSignature)

  lazy val dummyScriptWitness: P2WPKHWitnessV0 = {
    P2WPKHWitnessV0(dummyPartialSig.pubKey, dummyPartialSig.signature)
  }

  lazy val dummyAddress: BitcoinAddress = BitcoinAddress(
    "bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2")

  lazy val dummyDLCKeys: DLCPublicKeys =
    DLCPublicKeys(dummyKey, dummyAddress)

  lazy val dummyBlockHash: DoubleSha256DigestBE = DoubleSha256DigestBE(
    "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")

  val dummyPrevTx: BaseTransaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector.fill(2)(
      TransactionOutput(Satoshis(5000), P2WPKHWitnessSPKV0(dummyKey))),
    UInt32.zero)

  val dummyFundingInputs = Vector(
    DLCFundingInputP2WPKHV0(UInt64.zero,
                            dummyPrevTx,
                            UInt32.zero,
                            TransactionConstants.sequence),
    DLCFundingInputP2WPKHV0(UInt64.one,
                            dummyPrevTx,
                            UInt32.one,
                            TransactionConstants.sequence)
  )

  lazy val sampleOfferPayoutSerialId: UInt64 = DLCMessage.genSerialId()
  lazy val sampleOfferChangeSerialId: UInt64 = DLCMessage.genSerialId()

  lazy val sampleFundOutputSerialId: UInt64 =
    DLCMessage.genSerialId(Vector(sampleOfferChangeSerialId))

  lazy val sampleDLCOffer: DLCOffer = DLCOffer(
    contractInfo = sampleContractInfo,
    pubKeys = dummyDLCKeys,
    totalCollateral = Satoshis(5000),
    fundingInputs = Vector(dummyFundingInputs.head),
    changeAddress = dummyAddress,
    payoutSerialId = sampleOfferPayoutSerialId,
    changeSerialId = sampleOfferChangeSerialId,
    fundOutputSerialId = sampleFundOutputSerialId,
    feeRate = SatoshisPerVirtualByte(Satoshis(3)),
    timeouts = dummyTimeouts
  )

  lazy val sampleMultiNonceDLCOffer: DLCOffer =
    sampleDLCOffer.copy(contractInfo = multiNonceContractInfo)

  lazy val sampleDLCParamHash: Sha256DigestBE =
    DLCMessage.calcParamHash(sampleContractInfo, dummyTimeouts)

  lazy val dummyOutcomeSigs: Vector[(EnumOracleOutcome, ECAdaptorSignature)] =
    Vector(
      EnumOracleOutcome(Vector(sampleOracleInfo),
                        EnumOutcome(winStr)) -> ECAdaptorSignature.dummy,
      EnumOracleOutcome(Vector(sampleOracleInfo),
                        EnumOutcome(loseStr)) -> ECAdaptorSignature.dummy
    )

  lazy val dummyCETSigs: CETSignatures =
    CETSignatures(dummyOutcomeSigs, dummyPartialSig)

  lazy val sampleAcceptPayoutSerialId: UInt64 =
    DLCMessage.genSerialId(Vector(sampleOfferPayoutSerialId))

  lazy val sampleAcceptChangeSerialId: UInt64 = DLCMessage.genSerialId(
    Vector(sampleOfferChangeSerialId, sampleFundOutputSerialId))

  lazy val sampleDLCAccept: DLCAccept = DLCAccept(
    totalCollateral = Satoshis(5000),
    pubKeys = dummyDLCKeys,
    fundingInputs = Vector(dummyFundingInputs.last),
    changeAddress = dummyAddress,
    payoutSerialId = sampleAcceptPayoutSerialId,
    changeSerialId = sampleAcceptChangeSerialId,
    cetSigs = dummyCETSigs,
    negotiationFields = DLCAccept.NoNegotiationFields,
    tempContractId = sampleDLCOffer.tempContractId
  )

  lazy val dummyFundingSignatures: FundingSignatures = FundingSignatures(
    Vector(
      (TransactionOutPoint(dummyBlockHash, UInt32.zero), dummyScriptWitness)))

  lazy val sampleDLCSign: DLCSign =
    DLCSign(dummyCETSigs, dummyFundingSignatures, ByteVector.empty)

  lazy val sampleDLCDb: DLCDb = DLCDb(
    paramHash = sampleDLCParamHash,
    tempContractId = sampleDLCOffer.tempContractId,
    contractIdOpt = None,
    state = DLCState.Offered,
    isInitiator = true,
    account = HDAccount.fromPath(BIP32Path.fromString("m/84'/0'/0'")).get,
    keyIndex = 0,
    oracleSigsOpt = Some(Vector(sampleOracleLoseSig)),
    fundingOutPointOpt = None,
    fundingTxIdOpt = None,
    closingTxIdOpt = None,
    outcomesOpt = None,
    oraclesUsedOpt = None
  )

  def initDLC(
      fundedWalletA: FundedDLCWallet,
      fundedWalletB: FundedDLCWallet,
      contractInfo: ContractInfo)(implicit ec: ExecutionContext): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    val walletA = fundedWalletA.wallet
    val walletB = fundedWalletB.wallet

    val startOffer = System.currentTimeMillis()
    for {
      offer <- walletA.createDLCOffer(
        contractInfo = contractInfo,
        collateral = Satoshis(5000),
        feeRateOpt = None,
        locktime = dummyTimeouts.contractMaturity.toUInt32,
        refundLocktime = dummyTimeouts.contractTimeout.toUInt32
      )
      _ = logger.error(
        s"Created offer, it took=${System.currentTimeMillis() - startOffer}ms")
      acceptStart = System.currentTimeMillis()
      accept <- walletB.acceptDLCOffer(offer)
      _ = logger.error(
        s"Accept took ${System.currentTimeMillis() - acceptStart}ms")
      signStart = System.currentTimeMillis()
      sigs <- walletA.signDLC(accept)
      _ = logger.error(s"Sign took ${System.currentTimeMillis() - signStart}ms")
      addSigsStart = System.currentTimeMillis()
      _ <- walletB.addDLCSigs(sigs)

      _ = logger.error(
        s"Add sigs start took=${System.currentTimeMillis() - addSigsStart}ms")

      tx <- walletB.broadcastDLCFundingTx(sigs.contractId)
      _ <- walletA.processTransaction(tx, None)
    } yield {
      (InitializedDLCWallet(FundedDLCWallet(walletA)),
       InitializedDLCWallet(FundedDLCWallet(walletB)))
    }
  }

  case class InitializedDLCWallet(funded: FundedDLCWallet) {
    val wallet: DLCWallet = funded.wallet
  }

  def getInitialOffer(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[DLCOfferDb] = {
    wallet.dlcOfferDAO.findAll().map { all =>
      require(all.size == 1, "There should only be one dlc initialized")
      all.head
    }
  }

  def getContractId(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[ByteVector] = {
    wallet.dlcDAO.findAll().map { all =>
      require(all.size == 1, "There should only be one dlc initialized")
      all.head.contractIdOpt.get
    }
  }

  def verifyInput(
      transaction: Transaction,
      inputIndex: Long,
      prevOut: TransactionOutput): Boolean = {
    val sigComponent = WitnessTxSigComponent(
      transaction.asInstanceOf[WitnessTransaction],
      UInt32(inputIndex),
      prevOut,
      Policy.standardFlags
    )
    ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent))
  }

  def dlcExecutionTest(
      wallets: (InitializedDLCWallet, InitializedDLCWallet),
      asInitiator: Boolean,
      func: DLCWallet => Future[Transaction],
      expectedOutputs: Int)(implicit ec: ExecutionContext): Future[Boolean] = {
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet
    dlcExecutionTest(dlcA, dlcB, asInitiator, func, expectedOutputs)
  }

  def dlcExecutionTest(
      dlcA: DLCWallet,
      dlcB: DLCWallet,
      asInitiator: Boolean,
      func: DLCWallet => Future[Transaction],
      expectedOutputs: Int)(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      contractId <- getContractId(dlcA)
      fundingTx <- dlcB.getDLCFundingTx(contractId)
      tx <- if (asInitiator) func(dlcA) else func(dlcB)

      _ <- {
        if (asInitiator) dlcB.processTransaction(tx, None)
        else dlcA.processTransaction(tx, None)
      }

      dlcDb <- dlcA.dlcDAO.findByContractId(contractId)

      _ <- verifyProperlySetTxIds(dlcA)
      _ <- verifyProperlySetTxIds(dlcB)
    } yield {
      assert(tx.inputs.size == 1)
      assert(tx.outputs.size == expectedOutputs)
      assert(ScriptInterpreter.checkTransaction(tx))

      val fundOutputIndex = dlcDb.get.fundingOutPointOpt.get.vout.toInt
      val fundingOutput = fundingTx.outputs(fundOutputIndex)

      verifyInput(tx, 0, fundingOutput)
    }
  }

  def verifyProperlySetTxIds(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[Unit] = {
    for {
      contractId <- getContractId(wallet)
      dlcDbOpt <- wallet.dlcDAO.findByContractId(contractId)
    } yield {
      dlcDbOpt match {
        case None => fail()
        case Some(dlcDb) =>
          assert(dlcDb.fundingOutPointOpt.isDefined)
          assert(dlcDb.closingTxIdOpt.isDefined)
      }
    }
  }
}
