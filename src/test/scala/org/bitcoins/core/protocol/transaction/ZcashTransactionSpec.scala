package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.ZcashTransactionGenerator
import org.scalacheck.{ Prop, Properties }

class ZcashTransactionSpec extends Properties("ZcashTransactionSpec") {

  property("serialization symmetry") = {
    Prop.forAll(ZcashTransactionGenerator.preOverWinterTx) { tx =>
      ZcashTransaction.fromHex(tx.hex) == tx &&
        ZcashTransaction(tx.overwintered, tx.version, tx.versionGroupId,
          tx.inputs, tx.outputs, tx.lockTime, tx.expiryHeight, tx.joinSplits,
          tx.joinSplitPubKey, tx.joinSplitSig) == tx
    }
  }
}
