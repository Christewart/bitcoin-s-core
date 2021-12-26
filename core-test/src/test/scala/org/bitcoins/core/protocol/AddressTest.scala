package org.bitcoins.core.protocol

import org.bitcoins.core.config.MainNet
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.testkitcore.gen.AddressGenerator
import org.bitcoins.testkitcore.util.{BitcoinSUnitTest, TestUtil}

import scala.util.{Failure, Success}

class AddressTest extends BitcoinSUnitTest {

  behavior of "Address"

  it must "have serialization symmetry" in {
    forAll(AddressGenerator.address) { addr =>
      val fromSPK = Address
        .fromScriptPubKeyT(addr.scriptPubKey, addr.networkParameters)
      fromSPK match {
        case Success(newAddr)   => assert(newAddr.value == addr.value)
        case Failure(exception) => fail(exception.getMessage)
      }

      val fromStringT = Address.fromStringT(addr.value)
      fromStringT match {
        case Success(newAddr)   => assert(newAddr.value == addr.value)
        case Failure(exception) => fail(exception.getMessage)
      }
    }
  }

  it must "serialize a bech32 address correctly" in {
    TestUtil.bech32Address.toString must be(
      "bcrt1qq6w6pu6zq90az9krn53zlkvgyzkyeglzukyepf")
  }

  it must "calculate the correct descriptor" in {
    forAll(AddressGenerator.address) { addr =>
      assert(addr.descriptor == s"addr(${addr.value})")
    }
  }

  it must "generate a p2pkh address from a decompressed public key" in {
    //insert uncompressed pubkey here
    val hex =
      "044f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa385b6b1b8ead809ca67454d9683fcf2ba03456d6fe2c4abe2b07f0fbdbb2f1c1"
    val pubKey = ECPublicKey.fromHex(hex)
    val p2pkh = P2PKHAddress(pubKey, MainNet)
    println(s"address=${p2pkh.toString}")
  }
}
