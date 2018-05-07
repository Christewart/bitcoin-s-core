package org.bitcoins.core.protocol

import org.bitcoins.core.config.{ ZCashRegTest, ZCashTestNet }
import org.bitcoins.core.gen.AddressGenerator
import org.scalacheck.{ Gen, Prop, Properties }

class ZcashAddressSpec extends Properties("ZcashAddressSpec") {

  property("serialization symmetry for p2pkh") = {
    Prop.forAll(AddressGenerator.zcashP2PKHAddress) { p2pkh =>
      val bool = if (p2pkh.networkParameters == ZCashRegTest) {
        //since all the network parameters are the same between ZcashRegtest network & ZcashTestNet
        //we can't distinguish between the two
        ZcashP2PKHAddress.fromString(p2pkh.value) == ZcashP2PKHAddress(p2pkh.hash, ZCashTestNet)
      } else {
        ZcashP2PKHAddress.fromString(p2pkh.value) == p2pkh
      }
      bool && ZcashP2PKHAddress(p2pkh.hash, p2pkh.networkParameters) == p2pkh
    }
  }

  property("serialization symmetry for p2sh") = {
    Prop.forAll(AddressGenerator.zcashP2SHAddress) { p2sh =>
      val bool = if (p2sh.networkParameters == ZCashRegTest) {
        //since all the network parameters are the same between ZcashRegtest network & ZcashTestNet
        //we can't distinguish between the two
        ZcashP2SHAddress.fromString(p2sh.value) == ZcashP2SHAddress(p2sh.hash, ZCashTestNet)
      } else {
        ZcashP2SHAddress.fromString(p2sh.value) == p2sh
      }
      bool && ZcashP2SHAddress(p2sh.hash, p2sh.networkParameters) == p2sh
    }
  }

  property("serialization symmetry for all address types") = {
    Prop.forAll(AddressGenerator.zcashAddress) { addr =>
      val bool = if (addr.networkParameters == ZCashRegTest) {
        //since all the network parameters are the same between ZcashRegtest network & ZcashTestNet
        //we can't distinguish between the two
        ZcashAddress.fromString(addr.value).hash == addr.hash
      } else {
        ZcashAddress.fromString(addr.value) == addr
      }
      bool

    }
  }

}

