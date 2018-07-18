package org.bitcoins.core.protocol.ln

sealed abstract class LnInvoiceMultiplier {
  def char: Char
  def multi: Double
}

object LnInvoiceMultiplier {
  case object Milli extends LnInvoiceMultiplier {
    override def char = 'm'

    override def multi: Double = 0.001
  }

  //fill rest of them in..
}
