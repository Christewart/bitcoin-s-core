package org.bitcoins.core.dlc.testgen

import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.dlc.DLCTestUtil
import scodec.bits.ByteVector

object DLCTLVGen {

  val defaultAmt: Satoshis = CurrencyUnits.oneBTC.satoshis

  def hash(bytes: ByteVector = NumberUtil.randomBytes(32)): Sha256Digest = {
    CryptoUtil.sha256(bytes)
  }

  def getTempContractId(): Sha256Digest = hash()

  def genContractDescriptor(
      outcomes: Vector[String] = DLCTestUtil.genOutcomes(3),
      totalInput: CurrencyUnit = defaultAmt * 2): EnumContractDescriptor = {
    DLCTestUtil.genContractDescriptors(outcomes, totalInput)._1
  }

  def genOracleInfo(
      oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey,
      oracleRValue: SchnorrNonce = ECPublicKey.freshPublicKey.schnorrNonce,
      events: Vector[String] =
        Vector("dummy1", "dummy2")): EnumSingleOracleInfo = {
    EnumSingleOracleInfo(
      OracleAnnouncementV0TLV.dummyForEventsAndKeys(
        oraclePrivKey,
        oracleRValue,
        events.map(EnumOutcome.apply)))
  }

  def genEnumContractOraclePair(
      oraclePrivKey: ECPrivateKey,
      oracleRValue: SchnorrNonce,
      outcomes: Vector[String],
      totalInput: CurrencyUnit): ContractOraclePair.EnumPair = {
    val contract = genContractDescriptor(outcomes, totalInput)

    val oracleInfo = genOracleInfo(oraclePrivKey, oracleRValue, outcomes)
    ContractOraclePair.EnumPair(contract, oracleInfo)
  }

  def genContractInfo(
      oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey,
      oracleRValue: SchnorrNonce = ECPublicKey.freshPublicKey.schnorrNonce,
      outcomes: Vector[String] = DLCTestUtil.genOutcomes(3),
      totalInput: CurrencyUnit = defaultAmt * 2): ContractInfo = {
    val pair = genEnumContractOraclePair(oraclePrivKey,
                                         oracleRValue,
                                         outcomes,
                                         totalInput)

    SingleContractInfo(totalInput.satoshis, pair)
  }

  def p2wpkh(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey): P2WPKHWitnessSPKV0 = {
    P2WPKHWitnessSPKV0(pubKey)
  }

  def address(
      spk: ScriptPubKey = p2wpkh(),
      network: NetworkParameters = RegTest): BitcoinAddress = {
    spk match {
      case wspk: WitnessScriptPubKey => Bech32Address(wspk, network)
      case p2sh: P2SHScriptPubKey    => P2SHAddress(p2sh, network)
      case p2pkh: P2PKHScriptPubKey  => P2PKHAddress(p2pkh, network)
      case _: RawScriptPubKey =>
        throw new IllegalArgumentException(s"$spk is not valid for an address")
    }
  }

  def inputTransaction(
      input: CurrencyUnit = defaultAmt,
      spk: ScriptPubKey = p2wpkh()): Transaction = {
    BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector.empty,
      Vector(TransactionOutput(input * 2, spk)),
      UInt32.zero
    )
  }

  def outputReference(
      input: CurrencyUnit = defaultAmt,
      spk: ScriptPubKey =
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)): OutputReference = {
    val tx = inputTransaction(input, spk)
    OutputReference(TransactionOutPoint(tx.txIdBE, UInt32.zero),
                    tx.outputs.head)
  }

  def fundingInput(
      inputSerialId: UInt64 = DLCMessage.genSerialId(),
      prevTx: Transaction = inputTransaction(),
      prevTxVout: UInt32 = UInt32.zero,
      sequence: UInt32 = TransactionConstants.enableRBFSequence,
      maxWitnessLen: UInt16 = UInt16(107),
      redeemScriptOpt: Option[WitnessScriptPubKey] = None): DLCFundingInput = {
    DLCFundingInput(inputSerialId,
                    prevTx,
                    prevTxVout,
                    sequence,
                    maxWitnessLen,
                    redeemScriptOpt)
  }

  def adaptorSig: ECAdaptorSignature = {
    ECAdaptorSignature(
      ECPublicKey.freshPublicKey,
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement
    )
  }

  def ecdsaSig(sigHashByte: Boolean = true): ECDigitalSignature = {
    val sigWithoutSigHash = ECDigitalSignature.fromRS(
      ECPrivateKey.freshPrivateKey.fieldElement.toBigInteger,
      ECPrivateKey.freshPrivateKey.fieldElement.toBigInteger)

    if (sigHashByte) {
      ECDigitalSignature(sigWithoutSigHash.bytes :+ 0x01)
    } else {
      sigWithoutSigHash
    }
  }

  def partialSig(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      sigHashByte: Boolean = true): PartialSignature = {
    PartialSignature(pubKey, ecdsaSig(sigHashByte))
  }

  def p2wpkhWitnessV0(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      sigHashByte: Boolean = true): P2WPKHWitnessV0 = {
    P2WPKHWitnessV0(pubKey, ecdsaSig(sigHashByte))
  }

  def cetSigs(
      outcomes: Vector[EnumOutcome] =
        DLCTestUtil.genOutcomes(3).map(EnumOutcome.apply),
      oracleInfo: EnumSingleOracleInfo = genOracleInfo()): CETSignatures = {
    CETSignatures(
      outcomes.map(outcome =>
        EnumOracleOutcome(Vector(oracleInfo), outcome).sigPoint -> adaptorSig)
    )
  }

  def refundSigs(
      fundingPubKey: ECPublicKey =
        ECPublicKey.freshPublicKey): PartialSignature = {
    partialSig(fundingPubKey, sigHashByte = false)
  }

  def fundingSigs(
      outPoints: Vector[TransactionOutPoint] = Vector(
        outputReference().outPoint)): FundingSignatures = {
    FundingSignatures(outPoints.map(outpoint => outpoint -> p2wpkhWitnessV0()))
  }

  def dlcOffer(
      protocolVersionOpt: Option[Int] = DLCOfferTLV.currentVersionOpt,
      tempContractId: Sha256Digest = getTempContractId(),
      contractInfo: ContractInfo = genContractInfo(),
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      payoutSerialId: UInt64 = DLCMessage.genSerialId(),
      totalCollateral: Satoshis = defaultAmt,
      fundingInputs: Vector[DLCFundingInput] = Vector(fundingInput()),
      changeAddress: BitcoinAddress = address(),
      changeSerialId: UInt64 = DLCMessage.genSerialId(),
      fundOutputSerialId: UInt64 = DLCMessage.genSerialId(),
      feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte.one,
      contractMaturityBound: BlockTimeStamp = BlockTimeStamp(100),
      contractTimeout: BlockTimeStamp = BlockTimeStamp(200)): DLCOffer = {
    DLCOffer(
      protocolVersionOpt,
      tempContractId,
      contractInfo,
      DLCPublicKeys(fundingPubKey, payoutAddress),
      totalCollateral,
      fundingInputs,
      changeAddress,
      payoutSerialId,
      changeSerialId,
      fundOutputSerialId,
      feeRate,
      DLCTimeouts(contractMaturityBound, contractTimeout)
    )
  }

  def dlcOfferTLV(
      protocolVersionOpt: Option[Int] = DLCOfferTLV.currentVersionOpt,
      tempContractId: Sha256Digest = getTempContractId(),
      contractInfo: ContractInfo = genContractInfo(),
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      payoutSerialId: UInt64 = DLCMessage.genSerialId(),
      totalCollateral: Satoshis = defaultAmt,
      fundingInputs: Vector[DLCFundingInput] = Vector(fundingInput()),
      changeAddress: BitcoinAddress = address(),
      changeSerialId: UInt64 = DLCMessage.genSerialId(),
      fundOutputSerialId: UInt64 = DLCMessage.genSerialId(),
      feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte.one,
      contractMaturityBound: BlockTimeStamp = BlockTimeStamp(100),
      contractTimeout: BlockTimeStamp = BlockTimeStamp(200)): DLCOfferTLV = {
    dlcOffer(
      protocolVersionOpt,
      tempContractId,
      contractInfo,
      fundingPubKey,
      payoutAddress,
      payoutSerialId,
      totalCollateral,
      fundingInputs,
      changeAddress,
      changeSerialId,
      fundOutputSerialId,
      feeRate,
      contractMaturityBound,
      contractTimeout
    ).toTLV
  }

  def dlcAccept(
      protocolVersionOpt: Option[Int] = DLCOfferTLV.currentVersionOpt,
      totalCollateral: Satoshis = defaultAmt,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      payoutSerialId: UInt64 = DLCMessage.genSerialId(),
      fundingInputs: Vector[DLCFundingInput] = Vector(fundingInput()),
      changeAddress: BitcoinAddress = address(),
      changeSerialId: UInt64 = DLCMessage.genSerialId(),
      cetSignatures: CETSignatures = cetSigs(),
      refundSignatures: PartialSignature = refundSigs(),
      tempContractId: Sha256Digest = hash()): DLCAccept = {
    DLCAccept(
      protocolVersionOpt,
      totalCollateral,
      DLCPublicKeys(fundingPubKey, payoutAddress),
      fundingInputs,
      changeAddress,
      payoutSerialId,
      changeSerialId,
      cetSignatures,
      refundSignatures,
      DLCAccept.NoNegotiationFields,
      tempContractId
    )
  }

  def dlcAcceptTLV(
      protocolVersionOpt: Option[Int] = DLCOfferTLV.currentVersionOpt,
      totalCollateral: Satoshis = defaultAmt,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      payoutSerialId: UInt64 = DLCMessage.genSerialId(),
      fundingInputs: Vector[DLCFundingInput] = Vector(fundingInput()),
      changeAddress: BitcoinAddress = address(),
      changeSerialId: UInt64 = DLCMessage.genSerialId(),
      cetSignatures: CETSignatures = cetSigs(),
      refundSignatures: PartialSignature = refundSigs(),
      tempContractId: Sha256Digest = hash()): DLCAcceptTLV = {
    dlcAccept(
      protocolVersionOpt,
      totalCollateral,
      fundingPubKey,
      payoutAddress,
      payoutSerialId,
      fundingInputs,
      changeAddress,
      changeSerialId,
      cetSignatures,
      refundSignatures,
      tempContractId
    ).toTLV
  }

  def dlcAcceptFromOffer(
      offer: DLCOffer,
      overCollateral: Satoshis = Satoshis.zero,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      payoutSerialId: UInt64 = DLCMessage.genSerialId(),
      fundingInputs: Vector[DLCFundingInput] = Vector(fundingInput()),
      changeAddress: BitcoinAddress = address(),
      changeSerialId: UInt64 = DLCMessage.genSerialId()): DLCAccept = {
    val totalCollateral =
      offer.contractInfo.maxOffererPayout - offer.collateral + overCollateral

    val cetSignatures =
      cetSigs(
        offer.contractInfo.allOutcomes.map(
          _.asInstanceOf[EnumOracleOutcome].outcome),
        offer.contractInfo.oracleInfos.head.asInstanceOf[EnumSingleOracleInfo]
      )

    val refundSignatures = refundSigs(fundingPubKey)

    val tempContractId = offer.tempContractId

    DLCAccept(
      offer.protocolVersionOpt,
      totalCollateral.satoshis,
      DLCPublicKeys(fundingPubKey, payoutAddress),
      fundingInputs,
      changeAddress,
      payoutSerialId,
      changeSerialId,
      cetSignatures,
      refundSignatures,
      DLCAccept.NoNegotiationFields,
      tempContractId
    )
  }

  def dlcAcceptTLVFromOffer(
      offer: DLCOffer,
      overCollateral: Satoshis = Satoshis.zero,
      fundingPubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      payoutAddress: BitcoinAddress = address(),
      payoutSerialId: UInt64 = DLCMessage.genSerialId(),
      fundingInputs: Vector[DLCFundingInput] = Vector(fundingInput()),
      changeAddress: BitcoinAddress = address(),
      changeSerialId: UInt64 = DLCMessage.genSerialId()): DLCAcceptTLV = {
    dlcAcceptFromOffer(
      offer = offer,
      overCollateral = overCollateral,
      fundingPubKey = fundingPubKey,
      payoutAddress = payoutAddress,
      payoutSerialId = payoutSerialId,
      fundingInputs = fundingInputs,
      changeAddress = changeAddress,
      changeSerialId = changeSerialId
    ).toTLV
  }

  def dlcSign(
      protocolVersionOpt: Option[Int] = DLCOfferTLV.currentVersionOpt,
      cetSignatures: CETSignatures = cetSigs(),
      refundSignatures: PartialSignature = refundSigs(),
      fundingSignatures: FundingSignatures = fundingSigs(),
      contractId: ByteVector = hash().bytes): DLCSign = {
    DLCSign(protocolVersionOpt,
            cetSignatures,
            refundSignatures,
            fundingSignatures,
            contractId)
  }

  def dlcSignTLV(
      protocolVersionOpt: Option[Int] = DLCOfferTLV.currentVersionOpt,
      cetSignatures: CETSignatures = cetSigs(),
      refundSignatures: PartialSignature = refundSigs(),
      fundingSignatures: FundingSignatures = fundingSigs(),
      contractId: ByteVector = hash().bytes): DLCSignTLV = {
    dlcSign(
      protocolVersionOpt = protocolVersionOpt,
      cetSignatures = cetSignatures,
      refundSignatures = refundSignatures,
      fundingSignatures = fundingSignatures,
      contractId = contractId
    ).toTLV
  }

  def dlcSignFromOffer(
      offer: DLCOffer,
      contractId: ByteVector = hash().bytes): DLCSign = {
    val cetSignatures = {
      cetSigs(
        offer.contractInfo.allOutcomes.map(
          _.asInstanceOf[EnumOracleOutcome].outcome),
        offer.oracleInfos.head.asInstanceOf[EnumSingleOracleInfo]
      )
    }
    val refundSig = refundSigs(offer.pubKeys.fundingKey)
    val fundingSignatures = fundingSigs(offer.fundingInputs.map(_.outPoint))
    DLCSign(protocolVersionOpt = offer.protocolVersionOpt,
            cetSigs = cetSignatures,
            refundSig = refundSig,
            fundingSigs = fundingSignatures,
            contractId = contractId)
  }

  def dlcSignTLVFromOffer(
      offer: DLCOffer,
      contractId: ByteVector = hash().bytes): DLCSignTLV = {
    dlcSignFromOffer(offer, contractId).toTLV
  }

  def dlcSignFromOfferAndAccept(offer: DLCOffer, accept: DLCAccept): DLCSign = {
    val builder = DLCTxBuilder(offer, accept.withoutSigs)
    val fundingTx = builder.buildFundingTx
    val contractId = fundingTx.txIdBE.bytes.xor(accept.tempContractId.bytes)

    dlcSignFromOffer(offer, contractId)
  }

  def dlcSignTLVFromOfferAndAccept(
      offer: DLCOffer,
      accept: DLCAccept): DLCSignTLV = {
    dlcSignFromOfferAndAccept(offer, accept).toTLV
  }
}
