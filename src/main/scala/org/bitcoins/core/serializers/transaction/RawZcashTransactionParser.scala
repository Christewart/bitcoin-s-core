package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{JSDescription, TransactionInput, TransactionOutput, ZcashTransaction}
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}

/**
 * Created by str4d on 3/26/18.
 * For deserializing and re-serializing a Zcash transaction
 * https://github.com/zcash/zips/blob/master/zip-0202.rst
 */
sealed abstract class RawZcashTransactionParser extends RawBitcoinSerializer[ZcashTransaction] {
  val OVERWINTER_VERSION_GROUP_ID = UInt32(0x03C48270)

  val helper = RawSerializerHelper
  def read(bytes : List[Byte]): ZcashTransaction = {
    val header = UInt32(bytes.take(4).reverse)
    val overwintered = (header >> 31).toInt == 1
    val version = header & UInt32(0x7FFFFFFF)

    var versionGroupId = if (overwintered) UInt32(bytes.slice(4, 8).reverse) else UInt32.zero

    val isOverwinterV3 =
      overwintered &&
      versionGroupId == OVERWINTER_VERSION_GROUP_ID &&
      version.toInt == 3
    require(!overwintered || isOverwinterV3, "Unknown Zcash transaction format")

    val txInputBytes = bytes.slice(if (overwintered) 8 else 4, bytes.size)
    val (inputs,outputBytes) = helper.parseCmpctSizeUIntSeq(txInputBytes,RawTransactionInputParser.read(_))
    val (outputs,lockTimeBytes) = helper.parseCmpctSizeUIntSeq(outputBytes,
      RawTransactionOutputParser.read(_))
    val lockTime = UInt32(lockTimeBytes.take(4).reverse)
    val expiryHeight = if (isOverwinterV3) UInt32(lockTimeBytes.slice(4, 8).reverse) else UInt32.zero

    val txJoinSplitBytes = lockTimeBytes.slice(if (isOverwinterV3) 8 else 4, bytes.size)
    if (version.toInt >= 2) {
      val (jss,joinSplitPubKeyBytes) = helper.parseCmpctSizeUIntSeq(txJoinSplitBytes, RawJSDescriptionParser.read(_))
      if (jss.size > 0) {
        val joinSplitPubKey = joinSplitPubKeyBytes.take(32)
        val joinSplitSig = joinSplitPubKeyBytes.slice(32, 96)
        ZcashTransaction(overwintered,version,versionGroupId,inputs,outputs,lockTime,expiryHeight,jss,joinSplitPubKey,joinSplitSig)
      } else {
        ZcashTransaction(overwintered,version,versionGroupId,inputs,outputs,lockTime,expiryHeight,Nil,Nil,Nil)
      }
    } else {
      ZcashTransaction(overwintered,version,versionGroupId,inputs,outputs,lockTime,expiryHeight,Nil,Nil,Nil)
    }
  }

  def write(tx : ZcashTransaction): Seq[Byte] = {
    val version = ((if (tx.overwintered) UInt32.one << 31 else UInt32.zero) | tx.version).bytes.reverse
    val versionGroupId = tx.versionGroupId.bytes.reverse
    val inputs: Seq[Byte] = helper.writeCmpctSizeUInt[TransactionInput](tx.inputs,
      RawTransactionInputParser.write(_))
    val outputs: Seq[Byte] = helper.writeCmpctSizeUInt[TransactionOutput](tx.outputs,
      RawTransactionOutputParser.write(_))
    val lockTime = tx.lockTime.bytes.reverse
    val expiryHeight = tx.expiryHeight.bytes.reverse
    val jss: Seq[Byte] = helper.writeCmpctSizeUInt[JSDescription](tx.joinSplits,
      RawJSDescriptionParser.write(_))

    val isOverwinterV3 =
      tx.overwintered &&
      tx.versionGroupId == OVERWINTER_VERSION_GROUP_ID &&
      tx.version.toInt == 3

    version ++ (if (tx.overwintered) versionGroupId else Nil) ++
    inputs ++ outputs ++
    lockTime ++ (if (isOverwinterV3) expiryHeight else Nil) ++
    (if (tx.version.toInt >= 2) jss ++ (
      if (tx.joinSplits.size > 0) tx.joinSplitPubKey ++ tx.joinSplitSig else Nil
    ) else Nil)
  }
}

object RawZcashTransactionParser extends RawZcashTransactionParser
