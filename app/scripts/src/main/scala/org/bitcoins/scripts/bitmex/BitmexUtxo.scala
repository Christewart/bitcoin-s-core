package org.bitcoins.scripts.bitmex

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.script.descriptor.P2WPKHDescriptor

import scala.util.{Failure, Success, Try}

case class BitmexUtxo(JBitmexUtxo: JBitmexUtxo) {
  val addr_type: String = JBitmexUtxo.getAddr_type

  val script: ScriptPubKey = {
    val t = Try(ScriptPubKey.fromAsmHex(JBitmexUtxo.getScript))
    t match {
      case Success(script) => script
      case Failure(_)      =>
        //must be a descriptor
        P2WPKHDescriptor.fromString(JBitmexUtxo.getScript).scriptPubKey
    }
  }
  val balance: Satoshis = Satoshis(JBitmexUtxo.getBalance)

  override def toString: String = {
    s"BitmexUtxo($addr_type,$balance,$script)"
  }
}
