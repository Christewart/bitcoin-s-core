package org.bitcoins.wallet

import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.dlc.wallet.db.{
  DLCContactDb,
  DLCDb,
  IncomingDLCOfferDb
}
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.keymanager.BIP39KeyManagerApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.*
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.models.*
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.*
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.{DoubleSha256DigestBE, Sha256Digest}
import scodec.bits.ByteVector

import java.net.InetSocketAddress
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

class WalletNotInitialized extends Exception("The wallet is not initialized")

class WalletHolder(initWalletOpt: Option[DLCNeutrinoHDWalletApi])(implicit
    ec: ExecutionContext
) extends DLCNeutrinoHDWalletApi
    with BitcoinSLogger {

  @volatile private var walletOpt: Option[DLCNeutrinoHDWalletApi] =
    initWalletOpt

  private def wallet: DLCNeutrinoHDWalletApi = synchronized {
    walletOpt match {
      case Some(wallet) => wallet
      case None =>
        throw new WalletNotInitialized
    }
  }

  override def accountHandling: AccountHandlingApi = wallet.accountHandling

  override def rescanHandling: RescanHandlingApi = wallet.rescanHandling

  override def fundTxHandling: FundTransactionHandlingApi =
    wallet.fundTxHandling

  override def utxoHandling: UtxoHandlingApi = wallet.utxoHandling

  override def addressHandling: AddressHandlingApi = wallet.addressHandling

  override def transactionProcessing: TransactionProcessingApi =
    wallet.transactionProcessing
  def isInitialized: Boolean = synchronized {
    walletOpt.isDefined
  }

  def replaceWallet(
      newWallet: DLCNeutrinoHDWalletApi
  ): Future[DLCNeutrinoHDWalletApi] =
    synchronized {
      val oldWalletOpt = walletOpt
      walletOpt = None
      val res = for {
        _ <- {
          oldWalletOpt match {
            case Some(oldWallet) => oldWallet.stop()
            case None            => Future.unit
          }
        }
        _ <- newWallet.start()
      } yield {
        synchronized {
          walletOpt = Some(newWallet)
          newWallet
        }
      }

      res.failed.foreach(ex => logger.error("Cannot start wallet ", ex))

      res
    }

  private def delegate[T]
      : (DLCNeutrinoHDWalletApi => Future[T]) => Future[T] = {
    Future(wallet).flatMap[T](_)
  }

  override def getNewAddress(): Future[BitcoinAddress] = delegate(
    _.getNewAddress())

  override def getNewChangeAddress(): Future[BitcoinAddress] = delegate(
    _.getNewChangeAddress())

  override def processCompactFilters(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)]
  ): Future[NeutrinoHDWalletApi] = {
    delegate(_.processCompactFilters(blockFilters))
  }

  override def isRescanning(): Future[Boolean] = delegate(_.isRescanning())

  override lazy val nodeApi: NodeApi = wallet.nodeApi
  override lazy val chainQueryApi: ChainQueryApi = wallet.chainQueryApi
  override lazy val feeRateApi: FeeRateApi = wallet.feeRateApi
  override lazy val creationTime: Instant = wallet.creationTime

  override def start(): Future[WalletApi] = delegate(_.start())

  override def stop(): Future[WalletApi] = {
    val res = delegate(_.stop())

    res.onComplete { _ =>
      synchronized {
        walletOpt = None
      }
    }

    res
  }

  override def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]] =
    delegate(_.updateUtxoPendingStates())

  override def getConfirmedBalance(): Future[CurrencyUnit] = delegate(
    _.getConfirmedBalance()
  )

  override def getConfirmedBalance(tag: AddressTag): Future[CurrencyUnit] =
    delegate(_.getConfirmedBalance(tag))

  override def getUnconfirmedBalance(): Future[CurrencyUnit] = delegate(
    _.getUnconfirmedBalance()
  )

  override def getUnconfirmedBalance(tag: AddressTag): Future[CurrencyUnit] =
    delegate(_.getUnconfirmedBalance(tag))

  override def listTransactions(): Future[Vector[TransactionDb]] = {
    delegate(_.listTransactions())
  }
  override def listUtxos(): Future[Vector[SpendingInfoDb]] = delegate(
    _.listUtxos()
  )

  override def listUtxos(state: TxoState): Future[Vector[SpendingInfoDb]] =
    delegate(_.listUtxos(state))

  override def listUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    delegate(_.listUtxos(tag))
  }

  override def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]
  ): Future[Vector[SpendingInfoDb]] = delegate(_.markUTXOsAsReserved(utxos))

  override def markUTXOsAsReserved(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = delegate(_.markUTXOsAsReserved(tx))

  override def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]
  ): Future[Vector[SpendingInfoDb]] = delegate(_.unmarkUTXOsAsReserved(utxos))

  override def unmarkUTXOsAsReserved(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = delegate(_.unmarkUTXOsAsReserved(tx))

  override def isEmpty(): Future[Boolean] = delegate(_.isEmpty())

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRate: FeeUnit
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendFromOutPoints(outPoints, address, feeRate))

  override def sweepWallet(address: BitcoinAddress, feeRate: FeeUnit)(implicit
      ec: ExecutionContext
  ): Future[Transaction] = delegate(_.sweepWallet(address, feeRate))

  override def bumpFeeRBF(
      txId: DoubleSha256DigestBE,
      newFeeRate: FeeUnit
  ): Future[Transaction] = delegate(_.bumpFeeRBF(txId, newFeeRate))

  override def bumpFeeCPFP(
      txId: DoubleSha256DigestBE,
      feeRate: FeeUnit
  ): Future[Transaction] = delegate(_.bumpFeeCPFP(txId, feeRate))

  override def isChange(output: TransactionOutput): Future[Boolean] = delegate(
    _.isChange(output)
  )

  override def getSyncState(): Future[BlockSyncState] = delegate(
    _.getSyncState()
  )

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfo,
      collateral,
      feeRateOpt,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfo,
      collateral,
      feeRateOpt,
      locktime,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def acceptDLCOffer(
      dlcOffer: DLCMessage.DLCOffer,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCAccept] = delegate(
    _.acceptDLCOffer(
      dlcOffer,
      peerAddress,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCMessage.DLCSign] =
    delegate(_.signDLC(acceptTLV))

  override def signDLC(
      accept: DLCMessage.DLCAccept
  ): Future[DLCMessage.DLCSign] = delegate(_.signDLC(accept))

  override def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb] = delegate(
    _.addDLCSigs(signTLV)
  )

  override def addDLCSigs(sigs: DLCMessage.DLCSign): Future[DLCDb] = delegate(
    _.addDLCSigs(sigs)
  )

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] =
    delegate(_.getDLCFundingTx(contractId))

  override def broadcastDLCFundingTx(
      contractId: ByteVector
  ): Future[Transaction] = delegate(_.broadcastDLCFundingTx(contractId))

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Seq[OracleAttestmentTLV]
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSigs))

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSigs))

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] =
    delegate(_.executeDLCRefund(contractId))

  override def listDLCs(states: Vector[DLCState]): Future[Vector[DLCStatus]] = {
    delegate(_.listDLCs(states))
  }
  override def listDLCs(): Future[Vector[DLCStatus]] = delegate(_.listDLCs())

  override def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]] =
    delegate(_.findDLC(dlcId))

  override def findDLCByTemporaryContractId(
      tempContractId: Sha256Digest
  ): Future[Option[DLCStatus]] = delegate(
    _.findDLCByTemporaryContractId(tempContractId)
  )

  override def cancelDLC(dlcId: Sha256Digest): Future[Unit] = delegate(
    _.cancelDLC(dlcId)
  )

  override def getDLCOffer(
      dlcId: Sha256Digest
  ): Future[Option[DLCMessage.DLCOffer]] = delegate(_.getDLCOffer(dlcId))

  override def getWalletAccounting(): Future[DLCWalletAccounting] = delegate(
    _.getWalletAccounting()
  )

  override def registerIncomingDLCOffer(
      offerTLV: DLCOfferTLV,
      peer: Option[String],
      message: Option[String]
  ): Future[Sha256Digest] = delegate(
    _.registerIncomingDLCOffer(offerTLV, peer, message)
  )

  override def listIncomingDLCOffers(): Future[Vector[IncomingDLCOfferDb]] =
    delegate(_.listIncomingDLCOffers())

  override def rejectIncomingDLCOffer(offerHash: Sha256Digest): Future[Unit] =
    delegate(_.rejectIncomingDLCOffer(offerHash))

  override def findIncomingDLCOffer(
      offerHash: Sha256Digest
  ): Future[Option[IncomingDLCOfferDb]] = delegate(
    _.findIncomingDLCOffer(offerHash)
  )

  override def listDLCContacts(): Future[Vector[DLCContactDb]] = delegate(
    _.listDLCContacts()
  )

  override def addDLCContact(contact: DLCContactDb): Future[Unit] = delegate(
    _.addDLCContact(contact)
  )

  override def removeDLCContact(address: InetSocketAddress): Future[Unit] =
    delegate(_.removeDLCContact(address))

  override def findDLCContacts(alias: String): Future[Vector[DLCContactDb]] =
    delegate(_.findDLCContacts(alias))

  override def addDLCContactMapping(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress
  ): Future[Unit] = delegate(_.addDLCContactMapping(dlcId, contactId))

  override def removeDLCContactMapping(dlcId: Sha256Digest): Future[Unit] =
    delegate(_.removeDLCContactMapping(dlcId))

  override def listDLCsByContact(
      address: InetSocketAddress
  ): Future[Vector[DLCStatus]] = delegate(_.listDLCsByContact(address))

  override def keyManager: BIP39KeyManagerApi = wallet.keyManager

  def getConfirmedBalance(account: HDAccount): Future[CurrencyUnit] =
    delegate(_.accountHandling.getConfirmedBalance(account))

  def getUnconfirmedBalance(account: HDAccount): Future[CurrencyUnit] =
    delegate(_.accountHandling.getUnconfirmedBalance(account))

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendWithAlgo(address, amount, feeRate, algo, fromAccount, newTags)
  )

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendFromOutPoints(
      outPoints,
      address,
      amount,
      feeRate,
      fromAccount,
      newTags
    )
  )

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToAddress(address, amount, feeRate, fromAccount, newTags)
  )

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToAddresses(addresses, amounts, feeRate, fromAccount, newTags)
  )

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToOutputs(outputs, feeRate, fromAccount, newTags)
  )

  override def signPSBT(psbt: PSBT)(implicit
      ec: ExecutionContext
  ): Future[PSBT] = delegate(_.signPSBT(psbt))

  override def getSyncDescriptorOpt(): Future[Option[SyncHeightDescriptor]] =
    delegate(_.getSyncDescriptorOpt())

  override def getWalletName(): Future[String] = delegate(_.getWalletName())

  override def getInfo(): Future[WalletInfo] = delegate(_.getInfo())

  override def broadcastTransaction(transaction: Transaction): Future[Unit] =
    delegate(_.broadcastTransaction(transaction))

  override def getTransactionsToBroadcast: Future[Vector[Transaction]] = {
    delegate(_.getTransactionsToBroadcast)
  }

  override def getFeeRate(): Future[FeeUnit] = delegate(_.getFeeRate())

  override def getBalance()(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = delegate(_.getBalance())

  override def getBalance(tag: AddressTag)(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = delegate(_.getBalance(tag))

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendFromOutPoints(outPoints, address, amount, feeRateOpt)
  )

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendFromOutPoints(outPoints, address, feeRateOpt)
  )

  override def sweepWallet(address: BitcoinAddress)(implicit
      ec: ExecutionContext
  ): Future[Transaction] = delegate(_.sweepWallet(address))

  override def sweepWallet(
      address: BitcoinAddress,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sweepWallet(address, feeRateOpt)
  )

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendWithAlgo(address, amount, feeRateOpt, algo)
  )

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToAddress(address, amount, feeRateOpt)
  )

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToOutputs(outputs, feeRateOpt)
  )

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit]
  )(implicit ec: ExecutionContext): Future[Transaction] = delegate(
    _.sendToAddresses(addresses, amounts, feeRateOpt)
  )

  def getBalance(account: HDAccount)(implicit
      ec: ExecutionContext
  ): Future[CurrencyUnit] = delegate(_.accountHandling.getBalance(account))

  override def processCompactFilter(
      blockHash: DoubleSha256DigestBE,
      blockFilter: GolombFilter
  ): Future[NeutrinoHDWalletApi] =
    delegate(_.processCompactFilter(blockHash, blockFilter))

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendWithAlgo(address, amount, feeRate, algo, fromAccount))

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      algo: CoinSelectionAlgo,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendWithAlgo(address, amount, feeRateOpt, algo, fromAccount))

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendWithAlgo(address, amount, feeRate, algo))

  override def sendWithAlgo(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      algo: CoinSelectionAlgo,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendWithAlgo(address, amount, feeRate, algo, newTags))

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(
      _.sendFromOutPoints(outPoints, address, amount, feeRate, fromAccount)
    )

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(
      _.sendFromOutPoints(outPoints, address, amount, feeRateOpt, fromAccount)
    )

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendFromOutPoints(outPoints, address, amount, feeRate))

  override def sendFromOutPoints(
      outPoints: Vector[TransactionOutPoint],
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendFromOutPoints(outPoints, address, amount, feeRate, newTags))

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddress(address, amount, feeRate, fromAccount))

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddress(address, amount, feeRateOpt, fromAccount))

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddress(address, amount, feeRate))

  override def sendToAddress(
      address: BitcoinAddress,
      amount: CurrencyUnit,
      feeRate: FeeUnit,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddress(address, amount, feeRate, newTags))

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddresses(addresses, amounts, feeRate, fromAccount))

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddresses(addresses, amounts, feeRateOpt, fromAccount))

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddresses(addresses, amounts, feeRate))

  override def sendToAddresses(
      addresses: Vector[BitcoinAddress],
      amounts: Vector[CurrencyUnit],
      feeRate: FeeUnit,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToAddresses(addresses, amounts, feeRate, newTags))

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToOutputs(outputs, feeRate, fromAccount))

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRateOpt: Option[FeeUnit],
      fromAccount: AccountDb
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToOutputs(outputs, feeRateOpt, fromAccount))

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit,
      newTags: Vector[AddressTag]
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToOutputs(outputs, feeRate, newTags))

  override def sendToOutputs(
      outputs: Vector[TransactionOutput],
      feeRate: FeeUnit
  )(implicit ec: ExecutionContext): Future[Transaction] =
    delegate(_.sendToOutputs(outputs, feeRate))

  override def createDLCOffer(
      contractInfoTLV: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfoTLV,
      collateral,
      feeRateOpt,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def createDLCOffer(
      contractInfoTLV: ContractInfoTLV,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      peerAddressOpt: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCOffer] = delegate(
    _.createDLCOffer(
      contractInfoTLV,
      collateral,
      feeRateOpt,
      locktime,
      refundLT,
      peerAddressOpt,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def acceptDLCOffer(
      dlcOfferTLV: DLCOfferTLV,
      peerAddress: Option[InetSocketAddress],
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]
  ): Future[DLCMessage.DLCAccept] = delegate(
    _.acceptDLCOffer(
      dlcOfferTLV,
      peerAddress,
      externalPayoutAddressOpt,
      externalChangeAddressOpt
    )
  )

  override def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleAttestmentTLV
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSig))

  override def executeDLC(
      contractId: ByteVector,
      oracleSig: OracleSignatures
  ): Future[Option[Transaction]] =
    delegate(_.executeDLC(contractId, oracleSig))

  override def findByOutPoints(
      outPoints: Vector[TransactionOutPoint]
  ): Future[Vector[SpendingInfoDb]] = {
    delegate(_.findByOutPoints(outPoints))
  }

  override def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]
  ): Future[Vector[TransactionDb]] = {
    delegate(_.findByTxIds(txIds))
  }

  override def findOutputsBeingSpent(
      tx: Transaction
  ): Future[Vector[SpendingInfoDb]] = {
    delegate(_.findOutputsBeingSpent(tx))
  }

  override def findByScriptPubKey(
      scriptPubKey: ScriptPubKey
  ): Future[Vector[SpendingInfoDb]] = {
    delegate(_.findByScriptPubKey(scriptPubKey))
  }
}

object WalletHolder {

  def empty(implicit ec: ExecutionContext): WalletHolder = new WalletHolder(
    None
  )
}
