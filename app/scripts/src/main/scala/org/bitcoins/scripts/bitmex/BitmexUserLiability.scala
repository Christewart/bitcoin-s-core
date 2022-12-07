package org.bitcoins.scripts.bitmex

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.crypto.DoubleSha256DigestBE

case class BitmexUserLiability(hash: DoubleSha256DigestBE, balance: Satoshis) {

  override def toString: String = {
    s"BitmexUserLiability($hash,$balance)"
  }
}
