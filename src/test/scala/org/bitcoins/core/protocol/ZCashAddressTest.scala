package test.scala.org.bitcoins.core.protocol

import org.bitcoins.core.config.{ ZCashMainNet, ZCashTestNet }
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.protocol.P2PKHAddress
import org.scalatest.{ FlatSpec, MustMatchers }

class ZCashAddressTest extends FlatSpec with MustMatchers {
  "P2PKHAddress" must "be able to create a zcash address" in {
    val expectedAddress = "tmScMRF9AUZjUSswt5xuEFb6e8FMwrEVqy3"
    val privateKey = ECPrivateKey.fromWIFToPrivateKey("cR3T3g8pSdQRJWq9JPiLeC6tzXLLyUg2uvbtvJUScBMqsDZdHZ7x")
    val publicKey = privateKey.publicKey
    val network = ZCashTestNet
    val addr = P2PKHAddress(publicKey, network)
    addr.value must be(expectedAddress)
  }
}
