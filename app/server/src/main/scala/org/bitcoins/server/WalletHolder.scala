package org.bitcoins.server

import grizzled.slf4j.Logging
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.dlc.wallet.db.{
  DLCContactDb,
  DLCDb,
  IncomingDLCOfferDb
}
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet._
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.hd.{AddressType, HDAccount, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  DLCMessage,
  DLCStatus,
  OracleSignatures
}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.{
  DLCAcceptTLV,
  DLCOfferTLV,
  DLCSignTLV,
  OracleAttestmentTLV
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  AddressTagName,
  AddressTagType,
  TxoState
}
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  Sha256Digest
}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class WalletNotInitialized extends Exception("The wallet is not initialized")

class WalletHolder(implicit ec: ExecutionContext)
    extends AnyDLCHDWalletApi
    with Logging {

  @volatile private var wallet: AnyDLCHDWalletApi = _

  def isInitialized: Boolean = synchronized {
    wallet != null
  }

  def replaceWallet(newWallet: AnyDLCHDWalletApi): Future[AnyDLCHDWalletApi] =
    synchronized {
      val oldWallet = wallet
      wallet = null
      val res = for {
        _ <- if (oldWallet == null) Future.unit else oldWallet.stop()
        _ <- newWallet.start()
      } yield {
        synchronized {
          wallet = newWallet
          newWallet
        }
      }

      res.failed.foreach(ex => logger.error("Cannot start wallet ", ex))

      res
    }

  private def w: AnyDLCHDWalletApi = synchronized {
    if (wallet == null)
      throw new WalletNotInitialized
    else
      wallet
  }

  private def delegate[T] = Future(w).flatMap[T](_)

  override def processBlock(
      block: Block): Future[WalletApi with NeutrinoWalletApi] =
    delegate(_.processBlock(block))

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)]): Future[
    WalletApi with NeutrinoWalletApi] = delegate(
    _.processCompactFilters(blockFilters))

  override def getMatchingBlocks(
      scripts: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      batchSize: Int,
      parallelismLevel: Int)(implicit ec: ExecutionContext): Future[
    Vector[NeutrinoWalletApi.BlockMatchingResponse]] = delegate(
    _.getMatchingBlocks(scripts, startOpt, endOpt, batchSize, parallelismLevel))

  override def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int,
      useCreationTime: Boolean,
      force: Boolean)(implicit ec: ExecutionContext): Future[RescanState] =
    delegate(
      _.rescanNeutrinoWallet(startOpt,
                             endOpt,
                             addressBatchSize,
                             useCreationTime,
                             force))

  override def discoveryBatchSize(): Int = w.discoveryBatchSize()

  override lazy val nodeApi: NodeApi = w.nodeApi
  override lazy val chainQueryApi: ChainQueryApi = w.chainQueryApi
  override lazy val feeRateApi: FeeRateApi = w.feeRateApi
  override lazy val creationTime: Instant = w.creationTime

  override def start(): Future[WalletApi] = delegate(_.start())

  override def stop(): Future[WalletApi] = {
    val res = delegate(_.stop())

    res.onComplete { _ =>
      synchronized {
        wallet = null
      }
    }

    res
  }

  override def processTransaction(
      transaction: Transaction,
      blockHash: Option[DoubleSha256DigestBE]): Future[WalletApi] = delegate(
    _.processTransaction(transaction, blockHash))

  override def findTransaction(
      txId: DoubleSha256DigestBE): Future[Option[TransactionDb]] = delegate(
    _.findTransaction(txId))

  override def fundRawTransaction(
      destinations: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromTagOpt: Option[AddressTag],
      markAsReserved: Boolean): Future[Transaction] = delegate(
    _.fundRawTransaction(destinations, feeRate, fromTagOpt, markAsReserved))

  override def listTransactions(): Future[Vector[TransactionDb]] = delegate(
    _.listTransactions())

  override def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]] =
    delegate(_.updateUtxoPendingStates())

  override def getConfirmedBalance(): Future[CurrencyUnit] = delegate(
    _.getConfirmedBalance())

  override def getConfirmedBalance(tag: AddressTag): Future[CurrencyUnit] =
    delegate(_.getConfirmedBalance(tag))

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = delegate(
    _.getUnconfirmedBalance())

  override def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit] =
    delegate(_.getUnconfirmedBalance(tag))

  override def listUtxos(): Future[Vector[SpendingInfoDb]] = delegate(
    _.listUtxos())

  override def listUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]] =
    delegate(_.listUtxos(tag))

  override def listUtxos(state: TxoState): Future[Vector[SpendingInfoDb]] =
    delegate(_.listUtxos(state))

  override def listAddresses(): Future[Vector[AddressDb]] = delegate(
    _.listAddresses())

  override def listSpentAddresses(): Future[Vector[AddressDb]] = delegate(
    _.listSpentAddresses())

  override def listFundedAddresses(): Future[
    Vector[(AddressDb, CurrencyUnit)]] = delegate(_.listFundedAddresses())

  override def listUnusedAddresses(): Future[Vector[AddressDb]] = delegate(
    _.listUnusedAddresses())

  override def listScriptPubKeys(): Future[Vector[ScriptPubKeyDb]] = delegate(
    _.listScriptPubKeys())

  override def watchScriptPubKey(
      scriptPubKey: ScriptPubKey): Future[ScriptPubKeyDb] = delegate(
    _.watchScriptPubKey(scriptPubKey))

  override def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = delegate(
    _.markUTXOsAsReserved(utxos))

  override def markUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = delegate(
    _.markUTXOsAsReserved(tx))

  override def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = delegate(
    _.unmarkUTXOsAsReserved(utxos))

  override def unmarkUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = delegate(
    _.unmarkUTXOsAsReserved(tx))

  override def isEmpty(): Future[Boolean] = delegate(_.isEmpty())

  override def getNewAddress(addressType: AddressType): Future[BitcoinAddress] =
    delegate(_.getNewAddress(addressType))

  override def getNewAddress(): Future[BitcoinAddress] = delegate(
    _.getNewAddress())

  override def getNewAddress(
      addressType: AddressType,
      tags: Vector[AddressTag]): Future[BitcoinAddress] = delegate(
    _.getNewAddress(addressType, tags))

  override def getNewAddress(tags: Vector[AddressTag]): Future[BitcoinAddress] =
    delegate(_.getNewAddress(tags))

  override def getUnusedAddress(
      addressType: AddressType): Future[BitcoinAddress] = delegate(
    _.getUnusedAddress(addressType))

  override def getUnusedAddress: Future[BitcoinAddress] = delegate(
    _.getUnusedAddress)

  override def getAddressInfo(
      address: BitcoinAddress): Future[Option[AddressInfo]] = delegate(
    _.getAddressInfo(address))

  override def tagAddress(
      address: BitcoinAddress,
      tag: AddressTag): Future[AddressTagDb] = delegate(
    _.tagAddress(address, tag))

  override def getAddressTags(
      address: BitcoinAddress): Future[Vector[AddressTagDb]] = delegate(
    _.getAddressTags(address))

  override def getAddressTags(
      address: BitcoinAddress,
      tagType: AddressTagType): Future[Vector[AddressTagDb]] = delegate(
    _.getAddressTags(address, tagType))

  override def getAddressTags(): Future[Vector[AddressTagDb]] = delegate(
    _.getAddressTags())

  override def getAddressTags(
      tagType: AddressTagType): Future[Vector[AddressTagDb]] = delegate(
    _.getAddressTags(tagType))

  override def dropAddressTag(addressTagDb: AddressTagDb): Future[Int] =
    delegate(_.dropAddressTag(addressTagDb))

  override def dropAddressTagType(addressTagType: AddressTagType): Future[Int] =
    delegate(_.dropAddressTagType(addressTagType))

  override def dropAddressTagType(
      address: BitcoinAddress,
      addressTagType: AddressTagType): Future[Int] = delegate(
    _.dropAddressTagType(address, addressTagType))

  override def dropAddressTagName(
      address: BitcoinAddress,
      tagName: AddressTagName): Future[Int] = delegate(
    _.dropAddressTagName(address, tagName))

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit)(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendFromOutPoints(outPoints, address, feeRate))

  override def sweepWallet(address: BitcoinAddress, feeRate: FeeUnit)(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.sweepWallet(address, feeRate))

  override def bumpFeeRBF(
      txId: DoubleSha256DigestBE,
      newFeeRate: FeeUnit): Future[Transaction] = delegate(
    _.bumpFeeRBF(txId, newFeeRate))

  override def bumpFeeCPFP(
      txId: DoubleSha256DigestBE,
      feeRate: FeeUnit): Future[Transaction] = delegate(
    _.bumpFeeCPFP(txId, feeRate))

  override def isChange(output: TransactionOutput): Future[Boolean] = delegate(
    _.isChange(output))

  override def getSyncState(): Future[BlockSyncState] = delegate(
    _.getSyncState())

  override def isRescanning(): Future[Boolean] = delegate(_.isRescanning())

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[
    DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(contractInfo,
                     collateral,
                     feeRateOpt,
                     refundLT,
                     peerAddressOpt,
                     externalPayoutAddressOpt,
                     externalChangeAddressOpt))

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[
    DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(contractInfo,
                     collateral,
                     feeRateOpt,
                     locktime,
                     refundLT,
                     peerAddressOpt,
                     externalPayoutAddressOpt,
                     externalChangeAddressOpt))

  override def acceptDLCOffer(
      dlcOffer: DLCMessage.DLCOffer,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): Future[
    DLCMessage.DLCAccept] = delegate(
    _.acceptDLCOffer(dlcOffer,
                     peerAddress,
                     externalPayoutAddressOpt,
                     externalChangeAddressOpt))

  override def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCMessage.DLCSign] =
    delegate(_.signDLC(acceptTLV))

  override def signDLC(
      accept: DLCMessage.DLCAccept): Future[DLCMessage.DLCSign] = delegate(
    _.signDLC(accept))

  override def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb] = delegate(
    _.addDLCSigs(signTLV))

  override def addDLCSigs(sigs: DLCMessage.DLCSign): Future[DLCDb] = delegate(
    _.addDLCSigs(sigs))

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] =
    delegate(_.getDLCFundingTx(contractId))

  override def broadcastDLCFundingTx(
      contractId: ByteVector): Future[Transaction] = delegate(
    _.broadcastDLCFundingTx(contractId))

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Seq[OracleAttestmentTLV]): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSigs))

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSigs))

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] =
    delegate(_.executeDLCRefund(contractId))

  override def listDLCs(): Future[Vector[DLCStatus]] = delegate(_.listDLCs())

  override def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]] =
    delegate(_.findDLC(dlcId))

  override def findDLCByTemporaryContractId(
      tempContractId: Sha256Digest): Future[Option[DLCStatus]] = delegate(
    _.findDLCByTemporaryContractId(tempContractId))

  override def cancelDLC(dlcId: Sha256Digest): Future[Unit] = delegate(
    _.cancelDLC(dlcId))

  override def getDLCOffer(
      dlcId: Sha256Digest): Future[Option[DLCMessage.DLCOffer]] = delegate(
    _.getDLCOffer(dlcId))

  override def getWalletAccounting(): Future[DLCWalletAccounting] = delegate(
    _.getWalletAccounting())

  override def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]): Future[Sha256Digest] = delegate(
    _.registerIncomingDLCOffer(offerTLV, peer, message))

  override def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]] =
    delegate(_.listIncomingDLCOffers())

  override def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit] =
    delegate(_.rejectIncomingDLCOffer(offerHash))

  override def findIncomingDLCOffer(
      offerHash: Sha256Digest): Future[Option[IncomingDLCOfferDb]] = delegate(
    _.findIncomingDLCOffer(offerHash))

  override def listDLCContacts(): Future[Vector[DLCContactDb]] = delegate(
    _.listDLCContacts())

  override def addDLCContact(contact: DLCContactDb): Future[Unit] = delegate(
    _.addDLCContact(contact))

  override def removeDLCContact(address: InetSocketAddress): Future[Unit] =
    delegate(_.removeDLCContact(address))

  override def findDLCContacts(alias: String): Future[Vector[DLCContactDb]] =
    delegate(_.findDLCContacts(alias))

  override def addDLCContactMapping(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress): Future[Unit] = delegate(
    _.addDLCContactMapping(dlcId, contactId))

  override def removeDLCContactMapping(dlcId: Sha256Digest): Future[Unit] =
    delegate(_.removeDLCContactMapping(dlcId))

  override def listDLCsByContact(
      address: InetSocketAddress): Future[Vector[DLCStatus]] = delegate(
    _.listDLCsByContact(address))

  override def keyManager: BIP39KeyManagerApi = w.keyManager

  override def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] =
    delegate(_.getConfirmedBalance(account))

  override def getUnconfirmedBalance(account: HDAccount): Future[CurrencyUnit] =
    delegate(_.getUnconfirmedBalance(account))

  override def getNewChangeAddress(account: AccountDb): Future[BitcoinAddress] =
    delegate(_.getNewChangeAddress(account))

  override def getDefaultAccount(): Future[AccountDb] = delegate(
    _.getDefaultAccount())

  override def getDefaultAccountForType(
      addressType: AddressType): Future[AccountDb] = delegate(
    _.getDefaultAccountForType(addressType))

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendWithAlgo(address, amount, feeRate, algo, fromAccount, newTags))

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendFromOutPoints(outPoints,
                        address,
                        amount,
                        feeRate,
                        fromAccount,
                        newTags))

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToAddress(address, amount, feeRate, fromAccount, newTags))

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToAddresses(addresses, amounts, feeRate, fromAccount, newTags))

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag])(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToOutputs(outputs, feeRate, fromAccount, newTags))

  override def signPSBT(psbt: PSBT)(implicit
      ec: ExecutionContext): Future[PSBT] = delegate(_.signPSBT(psbt))

  override def makeOpReturnCommitment(
      message: String,
      hashMessage: Boolean,
      feeRate: FeeUnit,
      fromAccount: AccountDb)(implicit
      ec: ExecutionContext): Future[Transaction] = delegate(
    _.makeOpReturnCommitment(message, hashMessage, feeRate, fromAccount))

  override def listDefaultAccountUtxos(): Future[Vector[SpendingInfoDb]] =
    delegate(_.listDefaultAccountUtxos())

  override def listUtxos(account: HDAccount): Future[Vector[SpendingInfoDb]] =
    delegate(_.listUtxos(account))

  override def listUtxos(
      hdAccount: HDAccount,
      tag: AddressTag): Future[Vector[SpendingInfoDb]] = delegate(
    _.listUtxos(hdAccount))

  override def listUtxos(
      hdAccount: HDAccount,
      state: TxoState): Future[Vector[SpendingInfoDb]] = delegate(
    _.listUtxos(hdAccount, state))

  override def listAddresses(account: HDAccount): Future[Vector[AddressDb]] =
    delegate(_.listAddresses(account))

  override def listSpentAddresses(
      account: HDAccount): Future[Vector[AddressDb]] = delegate(
    _.listSpentAddresses(account))

  override def listFundedAddresses(
      account: HDAccount): Future[Vector[(AddressDb, CurrencyUnit)]] = delegate(
    _.listFundedAddresses(account))

  override def listUnusedAddresses(
      account: HDAccount): Future[Vector[AddressDb]] = delegate(
    _.listUnusedAddresses(account))

  override def clearAllUtxos(): Future[HDWalletApi] = delegate(
    _.clearAllUtxos())

  override def clearUtxos(account: HDAccount): Future[HDWalletApi] = delegate(
    _.clearUtxos(account))

  override def getAddress(
      account: AccountDb,
      chainType: HDChainType,
      addressIndex: Int): Future[AddressDb] = delegate(
    _.getAddress(account, chainType, addressIndex))

  override def listAccounts(): Future[Vector[AccountDb]] = delegate(
    _.listAccounts())

  override def createNewAccount(
      keyManagerParams: KeyManagerParams): Future[HDWalletApi] = delegate(
    _.createNewAccount(keyManagerParams))

  override def createNewAccount(
      hdAccount: HDAccount,
      keyManagerParams: KeyManagerParams): Future[HDWalletApi] = delegate(
    _.createNewAccount(hdAccount, keyManagerParams))

  override def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] =
    delegate(_.getSyncDescriptorOpt())

  override def getWalletName(): Future[Option[String]] = delegate(
    _.getWalletName())

  override def getInfo(): Future[WalletInfo] = delegate(_.getInfo())
}
