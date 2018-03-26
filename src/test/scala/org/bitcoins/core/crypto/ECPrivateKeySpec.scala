package org.bitcoins.core.crypto

import org.bitcoins.core.config._
import org.bitcoins.core.gen.{ ChainParamsGenerator, CryptoGenerators }
import org.scalacheck.{ Prop, Properties }

/**
 * Created by chris on 7/25/16.
 */
class ECPrivateKeySpec extends Properties("ECPrivateKeySpec") {

  property("Serialization symmetry for WIF format") =
    Prop.forAll(CryptoGenerators.privateKey, ChainParamsGenerator.networkParams) { (privKey, network) =>
      val wif = privKey.toWIF(network)
      //commenting out this portion of the test for now, multiple cryptocurrency networks share the
      //same private key WIF prefix, which means you can't distinguish what network a private key is meant
      //for. For instance ZCash copies the same network byte with bitcoin.
      /*      val isCorrectNetwork = network match {
        case bn : BitcoinNetwork => bn match {
          case MainNet => ECPrivateKey.parseNetworkFromWIF(wif).get == network
          case TestNet3 | RegTest => ECPrivateKey.parseNetworkFromWIF(wif).get == TestNet3
        }
        case zn: ZCashNetwork => zn match {
          case ZCashMainNet => ECPrivateKey.parseNetworkFromWIF(wif).get == zn
          case ZCashTestNet | ZCashRegTest => ECPrivateKey.parseNetworkFromWIF(wif).get == ZCashTestNet
        }
      }*/
      ECPrivateKey.fromWIFToPrivateKey(wif) == privKey
    }

  property("Serialization symmetry") =
    Prop.forAll(CryptoGenerators.privateKey) { privKey =>
      ECPrivateKey(privKey.hex) == privKey
    }

  property("unique key generation") =
    Prop.forAll(CryptoGenerators.privateKey, CryptoGenerators.privateKey) { (privKey1, privKey2) =>
      privKey1 != privKey2
    }
}
