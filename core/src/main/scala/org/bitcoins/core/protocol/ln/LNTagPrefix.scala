package org.bitcoins.core.protocol.ln

sealed abstract class LNTagPrefix {
  def value: String
}

/**
 * This defines the necessary Lightning Network Tag Prefix's, as specified in BOLT-11
 * https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
 */
object LNTagPrefix {

  case object PaymentHash extends LNTagPrefix {
    override def value: String = "p"
  }

  case object Description extends LNTagPrefix {
    override def value: String = "d"
  }

  case object SignaturePubKey extends LNTagPrefix {
    override def value: String = "n"
  }

  case object DescriptionHash extends LNTagPrefix {
    override def value: String = "h"
  }

  case object ExpiryTime extends LNTagPrefix {
    override def value: String = "x"
  }

  case object CtvlExpiry extends LNTagPrefix {
    override def value: String = "c"
  }

  case object FallbackAddress extends LNTagPrefix {
    override def value: String = "f"
  }

  case object RoutingInfo extends LNTagPrefix {
    override def value: String = "r"
  }

  case object None extends LNTagPrefix {
    override def value: String = ""
  }

}