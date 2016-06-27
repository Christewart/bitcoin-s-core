package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.gen.TransactionGenerators
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/24/16.
  */
class TransactionInputSpec extends Properties("TranactionInputSpec") with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(TransactionGenerators.inputs) { input =>
      logger.info("Input: " + input)
      logger.info("Input hex: " + input.hex)
      TransactionInput(input.hex).hex == input.hex

    }
}
