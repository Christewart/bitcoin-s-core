package org.bitcoins.core.protocol.dlc.execution

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.compute.CETCalculator
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.dlc.sign.DLCTxSigner
import org.bitcoins.core.protocol.tlv.{EnumOutcome, NumericDLCOutcomeType}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.Indexed
import org.bitcoins.core.wallet.utxo.{ECSignatureParams, P2WSHV0InputInfo}
import org.bitcoins.crypto.{AdaptorSign, ECAdaptorSignature, ECPublicKey}

import scala.util.{Success, Try}

/** Responsible for constructing SetupDLCs and DLCOutcomes */
case class DLCExecutor(signer: DLCTxSigner) {
  val builder: DLCTxBuilder = signer.builder
  val isInitiator: Boolean = signer.isInitiator

  /** Constructs the initiator's SetupDLC given the non-initiator's
    * CETSignatures which should arrive in a DLC accept message
    */
  def setupDLCOffer(
      cetSigs: CETSignatures,
      refundSig: PartialSignature): Try[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    setupDLC(cetSigs, refundSig, None, None)
  }

  /** Constructs the non-initiator's SetupDLC given the initiator's
    * CETSignatures and FundingSignatures which should arrive in
    * a DLC sign message
    */
  def setupDLCAccept(
      cetSigs: CETSignatures,
      refundSig: PartialSignature,
      fundingSigs: FundingSignatures,
      cetsOpt: Option[Vector[WitnessTransaction]]): Try[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    setupDLC(cetSigs, refundSig, Some(fundingSigs), cetsOpt)
  }

  /** Constructs a SetupDLC given the necessary signature information
    * from the counter-party.
    */
  def setupDLC(
      cetSigs: CETSignatures,
      refundSig: PartialSignature,
      fundingSigsOpt: Option[FundingSignatures],
      cetsOpt: Option[Vector[WitnessTransaction]]): Try[SetupDLC] = {
    if (!isInitiator) {
      require(fundingSigsOpt.isDefined,
              "Accepting party must provide remote funding signatures")
    }

    val CETSignatures(outcomeSigs) = cetSigs
    val msgs = Indexed(outcomeSigs.map(_._1))
    val cets = cetsOpt match {
      case Some(cets) => cets
      case None       => builder.buildCETs(msgs)
    }
    val cetInfos =
      cets.zip(outcomeSigs).map { case (cet, (msg, remoteAdaptorSig)) =>
        msg -> CETInfo(cet, remoteAdaptorSig)
      }

    for {
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.completeFundingTx(fundingSigs)
          case None              => Success(builder.buildFundingTx)
        }
      }
    } yield {
      val refundTx = signer.completeRefundTx(refundSig)

      SetupDLC(fundingTx, cetInfos, refundTx)
    }
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sigs: Vector[OracleSignatures]): CurrencyUnit = {
    signer.getPayout(sigs)
  }

  /** Computes closing transactions from a DLCSetup and a set of OracleSignatures.
    * The Vector[OracleSignatures] may contain more OracleSignatures than are needed.
    *
    * TODO: Test over-sharing of OracleSignatures
    */
  def executeDLC(
      dlcSetup: SetupDLC,
      oracleSigs: Vector[OracleSignatures]): ExecutedDLCOutcome = {
    val remoteFundingPubKey = if (isInitiator) {
      builder.acceptFundingKey
    } else {
      builder.offerFundingKey
    }

    DLCExecutor.executeDLC(dlcSetup.cets,
                           oracleSigs,
                           signer.fundingKey,
                           remoteFundingPubKey,
                           builder.contractInfo,
                           dlcSetup.fundingTx,
                           builder.fundOutputIndex)
  }

  def executeRefundDLC(dlcSetup: SetupDLC): RefundDLCOutcome = {
    val SetupDLC(fundingTx, _, refundTx) = dlcSetup
    RefundDLCOutcome(fundingTx, refundTx)
  }

  def executeRefundDLC(refundSig: PartialSignature): RefundDLCOutcome = {
    val refundTx = signer.completeRefundTx(refundSig)
    val fundingTx = signer.builder.buildFundingTx
    RefundDLCOutcome(fundingTx, refundTx)
  }
}

object DLCExecutor {

  /** Given DLC setup data and oracle signatures, computes the OracleOutcome and a fully signed CET.
    *
    * This function will fail if no threshold-sized subset of the oracle signatures corresponds to
    * a valid set of expected oracle signatures as per the oracle announcements in the ContractInfo.
    */
  def executeDLC(
      remoteCETInfos: Vector[(ECPublicKey, CETInfo)],
      oracleSigs: Vector[OracleSignatures],
      fundingKey: AdaptorSign,
      remoteFundingPubKey: ECPublicKey,
      contractInfo: ContractInfo,
      fundingTx: Transaction,
      fundOutputIndex: Int
  ): ExecutedDLCOutcome = {
    val singleOracles: Vector[SingleOracleInfo] = oracleSigs.map(_.oracle)

    val oracleInfoOpt: Option[OracleInfo] = {
      contractInfo.oracleInfos.find { oracleInfo =>
        oracleInfo.threshold <= oracleSigs.length &&
        singleOracles.forall(oracleInfo.singleOracleInfos.contains)
      }
    }

    val oracleInfo: OracleInfo = oracleInfoOpt.getOrElse(
      throw new IllegalArgumentException(
        s"Signatures do not correspond to any possible outcome! $oracleSigs"))

    val threshold = oracleInfo.threshold
    val sigCombinations: Vector[Vector[OracleSignatures]] = {
      CETCalculator.combinations(oracleSigs, threshold)
    }

    var oracleOutcomeOpt: Option[OracleOutcome] = None
    val sigsUsedOpt = sigCombinations.find { sigs =>
      oracleOutcomeOpt = contractInfo.findOutcome(sigs)
      if (oracleOutcomeOpt.isDefined) {
        //make sure we have the correct number of digits
        oracleOutcomeOpt.get.outcome match {
          case EnumOutcome(_) =>
            require(
              oracleSigs.forall(_.sigs.length == 1),
              s"Enum outcomes require 1 signature from oracles. Got=${oracleSigs
                .map(_.sigs.length)}")
          case n: NumericDLCOutcomeType =>
            require(
              oracleSigs.forall(_.sigs.length == n.digits.length),
              s"Numeric outcomes require the same number of sigs as nonces in the announcement, oracleSigs=${oracleSigs
                .map(_.sigs.length)} digits=${n.digits.length}"
            )
        }
      }
      oracleOutcomeOpt.isDefined
    }

    val msgAndCETInfoOpt: Option[(OracleOutcome, CETInfo)] =
      oracleOutcomeOpt.flatMap { msg =>
        remoteCETInfos
          .find(_._1 == msg.sigPoint)
          .map { case (_, info) => (msg, info) }
      }

    val (msg: OracleOutcome,
         ucet: WitnessTransaction,
         remoteAdaptorSig: ECAdaptorSignature) = {
      msgAndCETInfoOpt match {
        case Some((msg, CETInfo(ucet, remoteSig))) => (msg, ucet, remoteSig)
        case None =>
          throw new IllegalArgumentException(
            s"Signature does not correspond to any possible outcome! $oracleSigs")
      }
    }
    val sigsUsed: Vector[OracleSignatures] =
      sigsUsedOpt.get // Safe because msgOpt is defined if no throw

    val (fundingMultiSig, _) = DLCTxBuilder.buildFundingSPKs(
      Vector(fundingKey.publicKey, remoteFundingPubKey))

    val signingInfo: ECSignatureParams[P2WSHV0InputInfo] =
      DLCTxSigner.buildCETSigningInfo(fundOutputIndex = fundOutputIndex,
                                      fundingTx = fundingTx,
                                      fundingMultiSig = fundingMultiSig,
                                      fundingKey = fundingKey)

    val cet: WitnessTransaction = {
      DLCTxSigner.completeCET(
        outcome = msg,
        cetSigningInfo = signingInfo,
        fundingMultiSig = fundingMultiSig,
        fundingTx = fundingTx,
        ucet = ucet,
        remoteAdaptorSig = remoteAdaptorSig,
        remoteFundingPubKey = remoteFundingPubKey,
        oracleSigs = sigsUsed
      )
    }

    println(s"@@@@ outcome=${msg.outcome} @@@@")
    ExecutedDLCOutcome(fundingTx, cet, msg, sigsUsed)
  }
}
