package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

class OfferedHTLCSpec extends Properties("OfferedHTLCSpec") {

  property("serialization symmetry") = {
    Prop.forAllNoShrink(ScriptGenerators.offeredHTLC) { case (offeredHTLC,privKeys) =>
        OfferedHTLC(offeredHTLC.hex) == offeredHTLC &&
        OfferedHTLC(privKeys(0).publicKey, privKeys(1).publicKey, privKeys(2).publicKey,
          offeredHTLC.paymentHash) == offeredHTLC
    }
  }
}
