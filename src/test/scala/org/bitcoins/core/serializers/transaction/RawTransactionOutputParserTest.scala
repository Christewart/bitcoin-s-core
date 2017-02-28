
package org.bitcoins.core.serializers.transaction


import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.transaction.{EmptyTransactionOutput, TransactionOutput}
import org.bitcoins.core.script.bitwise.OP_EQUAL
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.crypto.OP_HASH160
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
class RawTransactionOutputParserTest extends FlatSpec with MustMatchers with RawTransactionOutputParser {

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxOutput = "02204e00000000000017a914eda8ae08b5c9f973f49543e90a7c292367b3337c87" +
    "197d2d000000000017a914be2319b9060429692ebeffaa3be38497dc5380c887"
  "RawTransactionOutputTest" must "read a serialized tx output" in {

    val txOutput : Seq[TransactionOutput] = read(rawTxOutput)
    val firstOutput = txOutput.head
    val secondOutput = txOutput(1)
    firstOutput.value must be (Satoshis(Int64(20000)))
    secondOutput.value must be (Satoshis(Int64(2981145)))
    firstOutput.scriptPubKey.asm must be (Seq(OP_HASH160, BytesToPushOntoStack(20),ScriptConstant("eda8ae08b5c9f973f49543e90a7c292367b3337c"), OP_EQUAL))
    secondOutput.scriptPubKey.asm must be (Seq(OP_HASH160,BytesToPushOntoStack(20), ScriptConstant("be2319b9060429692ebeffaa3be38497dc5380c8"), OP_EQUAL))
  }

  it must "seralialize a transaction output" in {
    val txOutput = read(rawTxOutput)
    write(txOutput) must be (rawTxOutput)
  }

  it must "serialize a single transaction output not in a sequence" in {
    val txOutputs = read(rawTxOutput)
    write(txOutputs.head) must be ("204e00000000000017a914eda8ae08b5c9f973f49543e90a7c292367b3337c87")
  }


  it must "serialize an older raw transaction output" in {
    //from this question
    //https://bitcoin.stackexchange.com/questions/2859/how-are-transaction-hashes-calculated
    val txOutput = "0100f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac"
    val output = RawTransactionOutputParser.read(txOutput)
    output.head.value must be (Satoshis(Int64(5000000000L)))
  }

  it must "read a two serialized ouptuts" in {
    //from this tx b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawTwoOutputs = "026c405d05000000001976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac809698000000000017a914af575bd77c5ce7eba3bd9ce6f89774713ae62c7987"
    val outputs = RawTransactionOutputParser.read(rawTwoOutputs)
    outputs.size must be (2)
  }

  it must "serialize the empty transaction output correctly" in {
    RawTransactionOutputParser.write(EmptyTransactionOutput) must be ("ffffffffffffffff00")

  }
}
