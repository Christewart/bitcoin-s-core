package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.{ BitcoinNetwork, NetworkParameters, ZCashNetwork }
import org.bitcoins.core.crypto.TxSigComponent
import org.bitcoins.core.currency.{ CurrencyUnit, CurrencyUnits, Satoshis }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder.{ BitcoinTxBuilderImpl, UTXOMap }
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer._
import org.bitcoins.core.wallet.utxo.{ BitcoinUTXOSpendingInfo, UTXOSpendingInfo, ZcashUTXOSpendingInfo }

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

/**
 * High level class to create a signed transaction that spends a set of
 * unspent transaction outputs.
 *
 * The most important method in this class is the 'sign' method. This will start the signing procedure for a
 * transaction and then return either a signed [[Transaction]] or a [[TxBuilderError]]
 *
 * For usage examples see TxBuilderSpec
 */
sealed abstract class TxBuilder {
  private val logger = BitcoinSLogger.logger

  /** The outputs which we are spending bitcoins to */
  def destinations: Seq[TransactionOutput]

  /** The [[ScriptPubKey]]'s we are spending bitcoins to */
  def destinationSPKs: Seq[ScriptPubKey] = destinations.map(_.scriptPubKey)

  /** A sequence of the amounts we are spending in this transaction */
  def destinationAmounts: Seq[CurrencyUnit] = destinations.map(_.value)

  /** The spent amount of bitcoins we are sending in the transaction, this does NOT include the fee */
  def destinationAmount: CurrencyUnit = destinationAmounts.fold(CurrencyUnits.zero)(_ + _)

  /** The total amount of satoshis that are able to be spent by this transaction */
  def creditingAmount: CurrencyUnit = utxos.map(_.output.value).fold(CurrencyUnits.zero)(_ + _)

  /** The largest possible fee in this transaction could pay */
  def largestFee: CurrencyUnit = creditingAmount - destinationAmount

  /**
   * The list of [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]s we are attempting to spend
   * and the signers, redeem scripts, and script witnesses that might be needed to spend this outpoint.
   * This information is dependent on what the [[ScriptPubKey]] type is we are spending. For isntance, if we are spending a
   * regular [[P2PKHScriptPubKey]], we do not need a redeem script or script witness.
   *
   * If we are spending a [[P2WPKHWitnessSPKV0]] we do not need a redeem script, but we need a [[ScriptWitness]]
   */
  def utxoMap: TxBuilder.UTXOMap

  def utxos: Seq[UTXOSpendingInfo] = utxoMap.values.toSeq

  /** This represents the rate, in [[FeeUnit]], we should pay for this transaction */
  def feeRate: FeeUnit

  /**
   * This is where all the money that is NOT sent to destination outputs is spent too.
   * If we don't specify a change output, a large miner fee may be paid as more than likely
   * the difference between [[creditingAmount]] and [[destinationAmount]] is not a market rate miner fee
   */
  def changeSPK: ScriptPubKey

  /**
   * The network that this [[org.bitcoins.core.wallet.builder.TxBuilder]] is signing a transaction for.
   * An example could be [[org.bitcoins.core.config.MainNet]]
   */
  def network: NetworkParameters

  /** The outpoints that we are using in this transaction */
  def outPoints: Seq[TransactionOutPoint] = utxoMap.keys.toSeq

  /** The redeem scripts that are needed in this transaction */
  def redeemScriptOpt: Seq[Option[ScriptPubKey]] = utxos.map(_.redeemScriptOpt).toSeq

  /** The script witnesses that are needed in this transaction */
  def scriptWitOpt: Seq[Option[ScriptWitness]] = utxos.map(_.scriptWitnessOpt).toSeq

  def sign(implicit ec: ExecutionContext): Future[Transaction]
}

/**
 * The [[org.bitcoins.core.wallet.builder.TxBuilder]] for the
 * bitcoin network(s) [[org.bitcoins.core.config.BitcoinNetwork]]
 */
sealed abstract class BitcoinTxBuilder extends TxBuilder {

  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants

  override def network: BitcoinNetwork

  override def utxoMap: BitcoinTxBuilder.UTXOMap

  override def sign(implicit ec: ExecutionContext): Future[Transaction] = sign(Policy.isRBFEnabled)

  /** Overloaded version of sign that skips passing a user invariant */
  def sign(isRBFEnabled: Boolean)(implicit ec: ExecutionContext): Future[Transaction] = {
    //trivially true function
    val f = (_: Seq[BitcoinUTXOSpendingInfo], _: Transaction) => true
    sign(f, isRBFEnabled)
  }

  def sign(invariants: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean)(implicit ec: ExecutionContext): Future[Transaction] = {
    sign(invariants, Policy.isRBFEnabled)
  }

  /**
   * Signs the given transaction and then returns a signed tx that spends
   * all of the given outputs.
   * Checks the given invariants when the signing process is done
   * An example of some invariants is that the fee on the signed transaction below a certain amount,
   * or that RBF is enabled on the signed transaction.
   *
   * @param invariants - invariants that should hold true when we are done signing the transaction
   * @param isRBFEnabled - if we should enable replace-by-fee on this transaction, see BIP125
   * @return the signed transaction, or a [[TxBuilderError]] indicating what went wrong when signing the tx
   */
  def sign(
    invariants: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean,
    isRBFEnabled: Boolean = false)(implicit ec: ExecutionContext): Future[Transaction] = {

    def loop(
      remaining: List[BitcoinUTXOSpendingInfo],
      txInProgress: Transaction): Future[Transaction] = remaining match {
      case Nil => Future.successful(txInProgress)
      case info :: t =>
        val partiallySigned = signAndAddInput(info, txInProgress)
        partiallySigned.flatMap(tx => loop(t, tx))
    }
    val utxos = utxoMap.values.toList
    val unsignedTxWit = TransactionWitness.fromWitOpt(scriptWitOpt)
    val lockTime = calcLockTime(utxos)
    val inputs = calcSequenceForInputs(utxos, isRBFEnabled)
    val changeOutput = TransactionOutput(CurrencyUnits.zero, changeSPK)
    val unsignedTxNoFee = lockTime.map { l =>
      unsignedTxWit match {
        case EmptyWitness => BaseTransaction(tc.validLockVersion, inputs, destinations ++ Seq(changeOutput), l)
        case wit: TransactionWitness => WitnessTransaction(tc.validLockVersion, inputs, destinations ++ Seq(changeOutput), l, wit)
      }
    }
    //NOTE: This signed version of the tx does NOT pay a fee, we are going to use this version to estimate the fee
    //and then deduct that amount of satoshis from the changeOutput, and then resign the tx.
    val signedTxNoFee: Future[Transaction] = Future.fromTry(unsignedTxNoFee).flatMap(utxnf => loop(utxos, utxnf))
    val signedTxWithFee = signedTxNoFee.flatMap { stxnf: Transaction =>
      val fee = feeRate.calc(stxnf)
      val change = creditingAmount - destinationAmount - fee
      val newChangeOutput = TransactionOutput(change, changeSPK)
      //if the change output is below the dust threshold after calculating the fee, don't add it
      //to the tx
      val newOutputs = if (newChangeOutput.value <= Policy.dustThreshold) {
        logger.debug("removing change output as value is below the dustThreshold")
        destinations
      } else {
        destinations ++ Seq(newChangeOutput)
      }
      val unsignedTx = unsignedTxWit match {
        case EmptyWitness => BaseTransaction(stxnf.version, inputs, newOutputs, stxnf.lockTime)
        case wit: TransactionWitness => WitnessTransaction(stxnf.version, inputs, newOutputs, stxnf.lockTime, wit)
      }
      //re-sign the tx with the appropriate change / fee
      val signedTx = loop(utxos, unsignedTx)
      signedTx.flatMap { tx =>
        val t: Try[Transaction] = if (invariants(utxos, tx)) {
          //final sanity checks
          TxBuilder.sanityChecks(this, tx) match {
            case Success(_) => Success(tx)
            case Failure(err) => Failure(err)
          }
        } else TxBuilderError.FailedUserInvariants
        Future.fromTry(t)
      }
    }
    signedTxWithFee
  }

  /**
   * This function creates a newly signed input, and then adds it to the unsigned transaction
   * @param utxo - the information needed to validly spend the given output
   * @param unsignedTx - the transaction that we are spending this output in
   * @return either the transaction with the signed input added, or a [[TxBuilderError]]
   */
  private def signAndAddInput(utxo: BitcoinUTXOSpendingInfo, unsignedTx: Transaction)(implicit ec: ExecutionContext): Future[Transaction] = {
    val outpoint = utxo.outPoint
    val output = utxo.output
    val signers = utxo.signers
    val redeemScriptOpt = utxo.redeemScriptOpt
    val scriptWitnessOpt = utxo.scriptWitnessOpt
    val hashType = utxo.hashType
    val idx = unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == outpoint)
    if (idx.isEmpty) {
      Future.fromTry(TxBuilderError.MissingOutPoint)
    } else {
      val inputIndex = UInt32(idx.get._2)
      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      output.scriptPubKey match {
        case _: P2PKScriptPubKey =>
          P2PKSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
        case _: P2PKHScriptPubKey => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
        case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case _: P2PKScriptPubKey => P2PKSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
            case _: P2PKHScriptPubKey => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
            case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
            case _: P2SHScriptPubKey => Future.fromTry(TxBuilderError.NestedP2SHSPK)
            case _: P2WSHWitnessSPKV0 | _: P2WPKHWitnessSPKV0 => Future.fromTry(TxBuilderError.NestedWitnessSPK)
            case _: CSVScriptPubKey | _: CLTVScriptPubKey
              | _: NonStandardScriptPubKey | _: WitnessCommitment | _: EscrowTimeoutScriptPubKey
              | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey => Future.fromTry(TxBuilderError.NoSigner)
          }
        case p2sh: P2SHScriptPubKey =>
          redeemScriptOpt match {
            case Some(redeemScript) =>
              if (p2sh != P2SHScriptPubKey(redeemScript)) {
                Future.fromTry(TxBuilderError.WrongRedeemScript)
              } else {
                val input = TransactionInput(outpoint, EmptyScriptSignature, oldInput.sequence)
                val updatedTx = unsignedTx match {
                  case btx: BaseTransaction =>
                    BaseTransaction(btx.version, unsignedTx.inputs.updated(inputIndex.toInt, input), btx.outputs, btx.lockTime)
                  case wtx: WitnessTransaction =>
                    WitnessTransaction(wtx.version, unsignedTx.inputs.updated(inputIndex.toInt, input), wtx.outputs, wtx.lockTime, wtx.witness)
                }
                val updatedOutput = TransactionOutput(output.value, redeemScript)
                val updatedUTXOInfo = BitcoinUTXOSpendingInfo(outpoint, updatedOutput, signers, None, scriptWitnessOpt, hashType)
                val signedTxEither = signAndAddInput(updatedUTXOInfo, updatedTx)
                signedTxEither.map { signedTx =>
                  val i = signedTx.inputs(inputIndex.toInt)
                  val p2sh = P2SHScriptSignature(i.scriptSignature, redeemScript)
                  val signedInput = TransactionInput(i.previousOutput, p2sh, i.sequence)
                  val signedInputs = signedTx.inputs.updated(inputIndex.toInt, signedInput)
                  signedTx match {
                    case btx: BaseTransaction =>
                      BaseTransaction(btx.version, signedInputs, btx.outputs, btx.lockTime)
                    case wtx: WitnessTransaction =>
                      WitnessTransaction(wtx.version, signedInputs, wtx.outputs, wtx.lockTime, wtx.witness)
                  }
                }
              }
            case None => Future.fromTry(TxBuilderError.NoRedeemScript)
          }

        case _: P2WPKHWitnessSPKV0 =>
          //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
          val unsignedWTx: WitnessTransaction = unsignedTx match {
            case btx: BaseTransaction => WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val result = P2WPKHSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
          result.map(_.transaction)
        case p2wshSPK: P2WSHWitnessSPKV0 =>
          //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
          val unsignedWTxFuture: Future[WitnessTransaction] = unsignedTx match {
            case btx: BaseTransaction => Future.successful(WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, EmptyWitness))
            case wtx: WitnessTransaction => Future.successful(wtx)
            case ztx: ZcashTransaction => Future.fromTry(TxBuilderError.WrongNetwork)
          }
          val p2wshScriptWit = scriptWitnessOpt match {
            case Some(wit) =>
              wit match {
                case EmptyScriptWitness | _: P2WPKHWitnessV0 => Future.fromTry(TxBuilderError.WrongWitness)
                case x: P2WSHWitnessV0 => Future.successful(x)
              }
            case None => Future.fromTry(TxBuilderError.NoWitness)
          }
          val redeemScriptEither = p2wshScriptWit.map(_.redeemScript)
          val result: Future[TxSigComponent] = unsignedWTxFuture.flatMap { unsignedWTx =>
            redeemScriptEither.flatMap { redeemScript =>
              if (P2WSHWitnessSPKV0(redeemScript) != p2wshSPK) {
                Future.fromTry(TxBuilderError.WrongWitness)
              } else {
                redeemScript match {
                  case _: P2PKScriptPubKey => P2PKSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
                  case _: P2PKHScriptPubKey => P2PKHSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
                  case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
                  case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 => Future.fromTry(TxBuilderError.NestedWitnessSPK)
                  case _: P2SHScriptPubKey => Future.fromTry(TxBuilderError.NestedP2SHSPK)
                  case lock: LockTimeScriptPubKey =>
                    lock.nestedScriptPubKey match {
                      case _: P2PKScriptPubKey => P2PKSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                      case _: P2PKHScriptPubKey => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                      case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                      case _: P2WPKHWitnessSPKV0 => P2WPKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                      case _: P2SHScriptPubKey => Future.fromTry(TxBuilderError.NestedP2SHSPK)
                      case _: P2WSHWitnessSPKV0 => Future.fromTry(TxBuilderError.NestedWitnessSPK)
                      case _: CSVScriptPubKey | _: CLTVScriptPubKey | _: EscrowTimeoutScriptPubKey
                        | _: NonStandardScriptPubKey | _: WitnessCommitment
                        | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
                        | _: EscrowTimeoutScriptPubKey => Future.fromTry(TxBuilderError.NoSigner)
                    }
                  case _: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey
                    | _: UnassignedWitnessScriptPubKey | _: EscrowTimeoutScriptPubKey =>
                    Future.fromTry(TxBuilderError.NoSigner)
                }
              }
            }
          }
          result.map(_.transaction)
        case _: NonStandardScriptPubKey | _: WitnessCommitment
          | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
          | _: EscrowTimeoutScriptPubKey => Future.fromTry(TxBuilderError.NoSigner)
      }
    }
  }

  /**
   * Returns a valid sequence number for the given [[ScriptNumber]]
   * A transaction needs a valid sequence number to spend a OP_CHECKSEQUENCEVERIFY script.
   * See BIP68/112 for more information
   * [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki]]
   * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki]]
   */
  private def solveSequenceForCSV(scriptNum: ScriptNumber): UInt32 = LockTimeInterpreter.isCSVLockByBlockHeight(scriptNum) match {
    case true =>
      //means that we need to have had scriptNum blocks bassed since this tx was included a block to be able to spend this output
      val blocksPassed = scriptNum.toLong & TransactionConstants.sequenceLockTimeMask.toLong
      val sequence = UInt32(blocksPassed)
      sequence
    case false =>
      //means that we need to have had 512 * n seconds passed since the tx was included in a block passed
      val n = scriptNum.toLong
      val sequence = UInt32(n & TransactionConstants.sequenceLockTimeMask.toLong)
      //set sequence number to indicate this is relative locktime
      sequence | TransactionConstants.sequenceLockTimeTypeFlag
  }

  /**
   * This helper function calculates the appropriate locktime for a transaction.
   * To be able to spend [[CLTVScriptPubKey]]'s you need to have the transaction's
   * locktime set to the same value (or higher) than the output it is spending.
   * See BIP65 for more info
   */
  private def calcLockTime(utxos: Seq[BitcoinUTXOSpendingInfo]): Try[UInt32] = {
    @tailrec
    def loop(remaining: Seq[BitcoinUTXOSpendingInfo], currentLockTime: UInt32): Try[UInt32] = remaining match {
      case Nil => Success(currentLockTime)
      case BitcoinUTXOSpendingInfo(outpoint, output, signers, redeemScriptOpt, scriptWitOpt, hashType) :: t => output.scriptPubKey match {
        case cltv: CLTVScriptPubKey =>
          val lockTime = if (cltv.locktime.toLong > UInt32.max.toLong || cltv.locktime.toLong < 0) {
            TxBuilderError.IncompatibleLockTimes
          } else Success(UInt32(cltv.locktime.toLong))
          val result = lockTime.flatMap { l: UInt32 =>
            if (currentLockTime < l) {
              val lockTimeThreshold = tc.locktimeThreshold
              if (currentLockTime < lockTimeThreshold && l >= lockTimeThreshold) {
                //means that we spend two different locktime types, one of the outputs spends a
                //OP_CLTV script by block height, the other spends one by time stamp
                TxBuilderError.IncompatibleLockTimes
              } else Success(l)
            } else Success(currentLockTime)
          }
          result
        case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
          if (redeemScriptOpt.isDefined) {
            //recursively call with redeem script as output script
            val o = TransactionOutput(output.value, redeemScriptOpt.get)
            val i = BitcoinUTXOSpendingInfo(outpoint, o, signers, None, scriptWitOpt, hashType)
            loop(i +: t, currentLockTime)
          } else if (scriptWitOpt.isDefined) {
            scriptWitOpt.get match {
              case EmptyScriptWitness => loop(t, currentLockTime)
              case _: P2WPKHWitnessV0 => loop(t, currentLockTime)
              case p2wsh: P2WSHWitnessV0 =>
                //recursively call with the witness redeem script as the script
                val o = TransactionOutput(output.value, p2wsh.redeemScript)
                val i = BitcoinUTXOSpendingInfo(outpoint, o, signers, redeemScriptOpt, None, hashType)
                loop(i +: t, currentLockTime)
            }
          } else {
            loop(t, currentLockTime)
          }
        case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
          | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey | _: WitnessCommitment
          | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey | _: CSVScriptPubKey
          | _: EscrowTimeoutScriptPubKey =>
          //non of these scripts affect the locktime of a tx
          loop(t, currentLockTime)
      }
    }
    loop(utxos, TransactionConstants.lockTime)
  }

  /**
   * This helper function calculates the appropriate sequence number for each transaction input.
   * [[CLTVScriptPubKey]] and [[CSVScriptPubKey]]'s need certain sequence numbers on the inputs
   * to make them spendable.
   * See BIP68/112 and BIP65 for more info
   */
  private def calcSequenceForInputs(utxos: Seq[UTXOSpendingInfo], isRBFEnabled: Boolean): Seq[TransactionInput] = {
    @tailrec
    def loop(remaining: Seq[UTXOSpendingInfo], accum: Seq[TransactionInput]): Seq[TransactionInput] = remaining match {
      case Nil => accum.reverse
      case BitcoinUTXOSpendingInfo(outpoint, output, signers, redeemScriptOpt, scriptWitOpt, hashType) :: t =>
        output.scriptPubKey match {
          case csv: CSVScriptPubKey =>
            val sequence = solveSequenceForCSV(csv.locktime)
            val i = TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, i +: accum)
          case _: CLTVScriptPubKey =>
            val sequence = UInt32.zero
            val i = TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, i +: accum)
          case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
            if (redeemScriptOpt.isDefined) {
              //recursively call with the redeem script in the output
              val o = TransactionOutput(output.value, redeemScriptOpt.get)
              val i = BitcoinUTXOSpendingInfo(outpoint, o, signers, None, scriptWitOpt, hashType)
              loop(i +: t, accum)
            } else if (scriptWitOpt.isDefined) {
              scriptWitOpt.get match {
                case EmptyScriptWitness | _: P2WPKHWitnessV0 => loop(t, accum)
                case p2wsh: P2WSHWitnessV0 =>
                  val o = TransactionOutput(output.value, p2wsh.redeemScript)
                  val i = BitcoinUTXOSpendingInfo(outpoint, o, signers, redeemScriptOpt, None, hashType)
                  loop(i +: t, accum)
              }
            } else loop(t, accum)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey
            | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey | _: WitnessCommitment
            | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey | _: EscrowTimeoutScriptPubKey =>
            //none of these script types affect the sequence number of a tx
            //the sequence only needs to be adjustd if we have replace by fee (RBF) enabled
            //see BIP125 for more information
            val sequence = if (isRBFEnabled) UInt32.zero else TransactionConstants.sequence
            val input = TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, input +: accum)
        }
    }
    val inputs = loop(utxos, Nil)
    inputs
  }
}

object TxBuilder {
  /** This contains all the information needed to create a valid [[TransactionInput]] that spends this utxo */
  type UTXOMap = Map[TransactionOutPoint, UTXOSpendingInfo]

  /** Runs various sanity checks on the final version of the signed transaction from TxBuilder */
  def sanityChecks(txBuilder: TxBuilder, signedTx: Transaction): Try[Unit] = {
    val sanityDestination = sanityDestinationChecks(txBuilder, signedTx)
    if (sanityDestination.isFailure) {
      sanityDestination
    } else {
      sanityAmountChecks(txBuilder, signedTx)
    }
  }

  /** Checks that we did not lose a [[TransactionOutput]] in the signing process of this transaction */
  def sanityDestinationChecks(txBuilder: TxBuilder, signedTx: Transaction): Try[Unit] = {
    //make sure we send coins to the appropriate destinations
    val isMissingDestination = txBuilder.destinations.map(o => signedTx.outputs.contains(o)).exists(_ == false)
    val hasExtraOutputs = if (signedTx.outputs.size == txBuilder.destinations.size) {
      false
    } else {
      //the extra output should be the changeOutput
      !(signedTx.outputs.size == (txBuilder.destinations.size + 1) &&
        signedTx.outputs.map(_.scriptPubKey).contains(txBuilder.changeSPK))
    }
    val spendingTxOutPoints = signedTx.inputs.map(_.previousOutput)
    val hasExtraOutPoints = txBuilder.outPoints.map(o => spendingTxOutPoints.contains(o)).exists(_ == false)
    if (isMissingDestination) {
      TxBuilderError.MissingDestinationOutput
    } else if (hasExtraOutputs) {
      TxBuilderError.ExtraOutputsAdded
    } else if (hasExtraOutPoints) {
      TxBuilderError.ExtraOutPoints
    } else {
      Success(Unit)
    }
  }

  /**
   * Checks that the [[TxBuilder.creditingAmount]] >= [[TxBuilder.destinationAmount]]
   * and then does a sanity check on the tx's fee
   */
  def sanityAmountChecks(txBuilder: TxBuilder, signedTx: Transaction): Try[Unit] = {
    val spentAmount: CurrencyUnit = signedTx.outputs.map(_.value).fold(CurrencyUnits.zero)(_ + _)
    val creditingAmount = txBuilder.creditingAmount
    val actualFee = creditingAmount - spentAmount
    val estimatedFee = txBuilder.feeRate * signedTx
    if (spentAmount > creditingAmount) {
      TxBuilderError.MintsMoney
    } else if (actualFee > txBuilder.largestFee) {
      TxBuilderError.HighFee
    } else if (signedTx.outputs.filterNot(_.scriptPubKey.asm.contains(OP_RETURN))
      .map(_.value).exists(_ < Policy.dustThreshold)) {
      TxBuilderError.OutputBelowDustThreshold
    } else {
      val feeResult = isValidFeeRange(estimatedFee, actualFee, txBuilder.feeRate)
      feeResult
    }
  }

  /**
   * Checks if the fee is within a 'valid' range
   * @param estimatedFee the estimated amount of fee we should pay
   * @param actualFee the actual amount of fee the transaction pays
   * @param feeRate the fee rate in satoshis/vbyte we paid per byte on this tx
   * @return
   */
  def isValidFeeRange(estimatedFee: CurrencyUnit, actualFee: CurrencyUnit, feeRate: FeeUnit): Try[Unit] = {
    //what the number '15' represents is the allowed variance -- in bytes -- between the size of the two
    //versions of signed tx. I believe the two signed version can vary in size because the digital
    //signature might have changed in size. It could become larger or smaller depending on the digital
    //signatures produced
    val acceptableVariance = 15 * feeRate.toLong
    val min = Satoshis(Int64(-acceptableVariance))
    val max = Satoshis(Int64(acceptableVariance))
    val difference = estimatedFee - actualFee
    if (difference <= min) {
      TxBuilderError.HighFee
    } else if (difference >= max) {
      TxBuilderError.LowFee
    } else {
      Success(Unit)
    }
  }
}

object BitcoinTxBuilder {
  type UTXOMap = Map[TransactionOutPoint, BitcoinUTXOSpendingInfo]

  private case class BitcoinTxBuilderImpl(
    destinations: Seq[TransactionOutput],
    utxoMap: UTXOMap,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork) extends BitcoinTxBuilder

  private val logger = BitcoinSLogger.logger

  /**
   * @param destinations where the money is going in the signed tx
   * @param utxos extra information needed to spend the outputs in the creditingTxs
   * @param feeRate the desired fee rate for this tx
   * @param changeSPK where we should send the change from the creditingTxs
   * @return either a instance of a [[TxBuilder]],
   *         from which you can call [[TxBuilder.sign]] to generate a signed tx,
   *         or a [[TxBuilderError]]
   */
  def apply(
    destinations: Seq[TransactionOutput],
    utxos: BitcoinTxBuilder.UTXOMap, feeRate: FeeUnit, changeSPK: ScriptPubKey, network: BitcoinNetwork): Future[BitcoinTxBuilder] = {
    if (feeRate.toLong <= 0) {
      Future.fromTry(TxBuilderError.LowFee)
    } else {
      Future.successful(BitcoinTxBuilderImpl(destinations, utxos, feeRate, changeSPK, network))
    }
  }

  def apply(
    destinations: Seq[TransactionOutput],
    utxos: Seq[BitcoinUTXOSpendingInfo], feeRate: FeeUnit, changeSPK: ScriptPubKey,
    network: BitcoinNetwork): Future[BitcoinTxBuilder] = {
    @tailrec
    def loop(utxos: Seq[UTXOSpendingInfo], accum: UTXOMap): UTXOMap = utxos match {
      case Nil => accum
      case h :: t =>
        val u = BitcoinUTXOSpendingInfo(h.outPoint, h.output, h.signers, h.redeemScriptOpt, h.scriptWitnessOpt, h.hashType)
        val result: BitcoinTxBuilder.UTXOMap = accum.updated(h.outPoint, u)
        loop(t, result)
    }
    val map = loop(utxos, Map.empty)
    BitcoinTxBuilder(destinations, map, feeRate, changeSPK, network)
  }
}

/**
 * This is a [[TxBuilder]] for the [[org.bitcoins.core.config.ZCashNetwork]]. Currently it is limited in functionality.
 * It can only spend a [[P2PKHScriptPubKey]], [[MultiSignatureScriptPubKey]] and a [[P2SHScriptPubKey]].
 * Whenever segwit gets incorporated into zcash, we will have to update this [[TxBuilder]] to accomodate it.
 * When we attempt to spend any utxos that are not one of the 3 types above, we will return a [[TxBuilderError.NoSigner]]
 */
sealed abstract class ZcashTxBuilder extends TxBuilder {
  private val tc = ZcashTxConstants
  private val logger = BitcoinSLogger.logger
  override def network: ZCashNetwork

  override def utxoMap: ZcashTxBuilder.UTXOMap

  override def utxos: Seq[ZcashUTXOSpendingInfo] = utxoMap.values.toSeq

  override def sign(implicit ec: ExecutionContext): Future[ZcashTransaction] = {
    val f = (_: ZcashTxBuilder.UTXOMap, _: Transaction) => true
    sign(f)
  }

  /**
   * Signs the given transaction and then returns a signed tx that spends
   * all of the given outputs.
   * Checks the given invariants when the signing process is done
   * An example of some invariants is that the fee on the signed transaction below a certain amount,
   * or that RBF is enabled on the signed transaction.
   *
   * @param invariants - invariants that should hold true when we are done signing the transaction
   * @return the signed transaction, or a [[TxBuilderError]] indicating what went wrong when signing the tx
   */
  def sign(invariants: (ZcashTxBuilder.UTXOMap, Transaction) => Boolean)(implicit ec: ExecutionContext): Future[ZcashTransaction] = {
    def loop(
      remaining: List[ZcashUTXOSpendingInfo],
      txInProgress: Future[ZcashTransaction]): Future[ZcashTransaction] = remaining match {
      case Nil => txInProgress
      case utxo :: t =>
        val partiallySigned = txInProgress.flatMap(t => signAndAddInput(utxo, t))
        loop(t, partiallySigned)
    }
    val lockTime = tc.lockTime
    val inputs = utxos.map(u => TransactionInput(u.outPoint, EmptyScriptSignature, tc.sequence))
    val changeOutput = TransactionOutput(CurrencyUnits.zero, changeSPK)
    val unsignedTxNoFee = ZcashTransaction(tc.version, inputs, destinations ++ Seq(changeOutput), lockTime).get
    //NOTE: This signed version of the tx does NOT pay a fee, we are going to use this version to estimate the fee
    //and then deduct that amount of satoshis from the changeOutput, and then resign the tx.
    val signedTxNoFeeFuture = loop(utxos.toList, Future.successful(unsignedTxNoFee))
    signedTxNoFeeFuture.flatMap { stxnf: ZcashTransaction =>
      val fee = feeRate.calc(stxnf)
      val newChangeOutput = TransactionOutput(creditingAmount - destinationAmount - fee, changeSPK)
      //if the change output is below the dust threshold after calculating the fee, don't add it
      //to the tx
      val newOutputs = if (newChangeOutput.value <= Policy.dustThreshold) {
        logger.debug("removing change output as value is below the dustThreshold")
        destinations
      } else {
        destinations ++ Seq(newChangeOutput)
      }
      val z = stxnf
      val unsignedTx = ZcashTransaction(z.version, z.versionGroupId, inputs, newOutputs, z.lockTime,
        z.expiryHeight, z.joinSplits, z.joinSplitPubKey, z.joinSplitSig)
      //re-sign the tx with the appropriate change / fee
      val signedTx = loop(utxos.toList, Future.successful(unsignedTx))
      signedTx.flatMap { tx =>
        if (invariants(utxoMap, tx)) {
          //final sanity checks
          val err = TxBuilder.sanityChecks(this, tx)
          if (err.isFailure) Future.failed[ZcashTransaction](err.failed.get)
          else Future.successful(tx)
        } else Future.fromTry(TxBuilderError.FailedUserInvariants)
      }
    }
  }

  /**
   * This function creates a newly signed input, and then adds it to the unsigned transaction
   * @param utxo - the information needed to validly spend the given output
   * @param unsignedTx - the transaction that we are spending this output in
   * @return either the transaction with the signed input added, or a [[TxBuilderError]]
   */
  private def signAndAddInput(utxo: ZcashUTXOSpendingInfo, unsignedTx: ZcashTransaction)(implicit ec: ExecutionContext): Future[ZcashTransaction] = {
    val outpoint = utxo.outPoint
    val output = utxo.output
    val signers = utxo.signers
    val redeemScriptOpt = utxo.redeemScriptOpt
    val scriptWitnessOpt = utxo.scriptWitnessOpt
    val hashType = utxo.hashType
    val idx = unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == outpoint)
    if (idx.isEmpty) {
      Future.fromTry(TxBuilderError.MissingOutPoint)
    } else {
      val inputIndex = UInt32(idx.get._2)
      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      val result: Future[Transaction] = output.scriptPubKey match {
        case _: P2PKHScriptPubKey => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
        case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType).map(_.transaction)
        case p2sh: P2SHScriptPubKey =>
          redeemScriptOpt match {
            case Some(redeemScript) =>
              if (p2sh != P2SHScriptPubKey(redeemScript)) {
                Future.fromTry(TxBuilderError.WrongRedeemScript)
              } else {
                val input = TransactionInput(outpoint, EmptyScriptSignature, oldInput.sequence)
                val updatedTx: ZcashTransaction = unsignedTx match {
                  case ztx: ZcashTransaction =>
                    ZcashTransaction(ztx.version, ztx.versionGroupId, unsignedTx.inputs.updated(inputIndex.toInt, input),
                      ztx.outputs, ztx.lockTime, ztx.expiryHeight, ztx.joinSplits, ztx.joinSplitPubKey, ztx.joinSplitSig)
                }
                val updatedOutput = TransactionOutput(output.value, redeemScript)
                val signedTxFuture: Future[ZcashTransaction] = signAndAddInput(ZcashUTXOSpendingInfo(outpoint, updatedOutput, signers, None,
                  scriptWitnessOpt, hashType), updatedTx)

                signedTxFuture.map { signedTx =>
                  val i = signedTx.inputs(inputIndex.toInt)
                  val p2sh = P2SHScriptSignature(i.scriptSignature, redeemScript)
                  val signedInput = TransactionInput(i.previousOutput, p2sh, i.sequence)
                  val signedInputs = signedTx.inputs.updated(inputIndex.toInt, signedInput)
                  signedTx match {
                    case ztx: ZcashTransaction =>
                      ZcashTransaction(ztx.version, ztx.versionGroupId, signedInputs, ztx.outputs, ztx.lockTime,
                        ztx.expiryHeight, ztx.joinSplits, ztx.joinSplitPubKey, ztx.joinSplitSig)
                  }
                }
              }
            case None => Future.fromTry(TxBuilderError.NoRedeemScript)
          }
        case _: P2PKScriptPubKey | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey
          | _: CSVScriptPubKey | _: CLTVScriptPubKey | _: NonStandardScriptPubKey | _: WitnessCommitment
          | _: EscrowTimeoutScriptPubKey | _: UnassignedWitnessScriptPubKey
          | EmptyScriptPubKey => Future.fromTry(TxBuilderError.NoSigner)
      }
      result.flatMap {
        case ztx: ZcashTransaction => Future.successful(ztx)
        case btx: BaseTransaction =>
          val ztxTry = ZcashTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime)
          ztxTry match {
            case Success(ztx) => Future.successful(ztx)
            case Failure(_) => Future.fromTry(TxBuilderError.BadZcashTx)
          }
        case wtx: WitnessTransaction =>
          //cannot have a witness trasnaction in ZCash, they have not integrated segwit support yet
          Future.fromTry(TxBuilderError.UnknownError)
      }
    }
  }
}

object ZcashTxBuilder {
  type UTXOMap = Map[TransactionOutPoint, ZcashUTXOSpendingInfo]
  private case class ZcashTxBuilderImpl(
    destinations: Seq[TransactionOutput],
    utxoMap: UTXOMap,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: ZCashNetwork) extends ZcashTxBuilder
  /**
   * @param destinations where the money is going in the signed tx
   * @param utxos extra information needed to spend the outputs in the creditingTxs
   * @param feeRate the desired fee rate for this tx
   * @param changeSPK where we should send the change from the creditingTxs
   * @return either a instance of a [[TxBuilder]],
   *         from which you can call [[TxBuilder.sign]] to generate a signed tx,
   *         or a [[TxBuilderError]]
   */
  def apply(
    destinations: Seq[TransactionOutput],
    utxos: ZcashTxBuilder.UTXOMap, feeRate: FeeUnit, changeSPK: ScriptPubKey,
    network: ZCashNetwork): Future[ZcashTxBuilder] = {
    if (feeRate.toLong <= 0) {
      Future.fromTry(TxBuilderError.LowFee)
    } else {
      Future.successful(ZcashTxBuilderImpl(destinations, utxos, feeRate, changeSPK, network))
    }
  }

  def apply(
    destinations: Seq[TransactionOutput],
    utxos: Seq[ZcashUTXOSpendingInfo], feeRate: FeeUnit, changeSPK: ScriptPubKey,
    network: ZCashNetwork): Future[ZcashTxBuilder] = {
    @tailrec
    def loop(utxos: Seq[UTXOSpendingInfo], accum: UTXOMap): UTXOMap = utxos match {
      case Nil => accum
      case h :: t =>
        val u = ZcashUTXOSpendingInfo(h.outPoint, h.output, h.signers, h.redeemScriptOpt, h.scriptWitnessOpt, h.hashType)
        val result: ZcashTxBuilder.UTXOMap = accum.updated(h.outPoint, u)
        loop(t, result)
    }
    val map = loop(utxos, Map.empty)
    ZcashTxBuilder(destinations, map, feeRate, changeSPK, network)
  }
}