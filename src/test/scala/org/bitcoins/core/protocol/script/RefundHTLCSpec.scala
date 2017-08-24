package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

class RefundHTLCSpec extends Properties("RefundHTLCSpec") {

  property("serialization symmetry") = {
    Prop.forAllNoShrink(ScriptGenerators.refundHTLC) { case (refundHTLC,privKeys) =>
        RefundHTLC(refundHTLC.bytes) == refundHTLC &&
        RefundHTLC(privKeys(0).publicKey,refundHTLC.locktime,privKeys(1).publicKey) == refundHTLC
    }
  }

}
