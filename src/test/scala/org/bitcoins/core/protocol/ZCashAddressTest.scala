package org.bitcoins.core.protocol

import org.bitcoins.core.config.{ ZCashMainNet, ZCashRegTest, ZCashTestNet }
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.script.{ MultiSignatureScriptPubKey, P2SHScriptPubKey }
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{ FlatSpec, MustMatchers }

import scala.util.Try

class ZCashAddressTest extends FlatSpec with MustMatchers {
  val encode = BitcoinSUtil.encodeHex(_: Seq[Byte])
  "P2PKHAddress" must "be able to create a zcash address" in {
    val expectedAddress = "tmScMRF9AUZjUSswt5xuEFb6e8FMwrEVqy3"
    val privateKey = ECPrivateKey.fromWIFToPrivateKey("cR3T3g8pSdQRJWq9JPiLeC6tzXLLyUg2uvbtvJUScBMqsDZdHZ7x")
    val publicKey = privateKey.publicKey
    val network = ZCashTestNet
    val addr = ZcashP2PKHAddress(publicKey, network)
    addr.value must be(expectedAddress)
  }

  it must "generate a valid multisig script and p2sh address" in {
    val privKey1 = ECPrivateKey.fromWIFToPrivateKey("cVz2nr2PXxXS6VpW2qvPE6tYjV4CfcSYhW6A3ezPJZfTV2wyJNCS")
    val privKey2 = ECPrivateKey.fromWIFToPrivateKey("cUS9eReHbKJKsMwu3uLqByrvyggWJZNp9tK1w3fWoYD5KjojdAHD")
    val privKey3 = ECPrivateKey.fromWIFToPrivateKey("cTurnj6bXgpuGqw9T3TKvDo839PqxXV3GYtwmBUeXG3x98cv8CvB")
    val privKeys = Seq(privKey1, privKey2, privKey3)
    val pubKeys = privKeys.map(_.publicKey)
    val multisig = MultiSignatureScriptPubKey(2, pubKeys)
    val p2sh = P2SHScriptPubKey(multisig)
    val addr = ZcashP2SHAddress(p2sh, ZCashRegTest)
    encode(multisig.asmBytes) must be("522103bb4385fff73754fe1d5be8799ef488677a170ec3698925970ee7c15ba950f7f821039764988dfa780ffd0506b806e7de4e3c1ebe1a281956be0c032c28b7a05dea2621020a0c850cbafc5011b10b54435b278090d32e6e15a9b5c454c662a1fc71ab6e4953ae")
    addr.value must be("t2QsZbrSWMmoEmBaHXcADAKAiMKap4cWZ82")
  }

  it must "fail to parse a z address" in {
    val str = "ztsEZYnaG9S2fMKMdBQ9nYTN93SwSPZCUScGy7JW4Meiy4mXAHC8ZgyqCWZxrPy5uvtGUpvZENRumknmnJ8kTkq1kxJNYfX"
    val addr = Try(ZcashAddress.fromString(str))
    addr.isFailure must be(true)
  }
}
