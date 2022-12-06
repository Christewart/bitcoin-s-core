package org.bitcoins.scripts.bitmex

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.script.ScriptPubKey

case class BitmexUtxo(JBitmexUtxo: JBitmexUtxo) {
  val addr_type: String = JBitmexUtxo.getAddr_type

  val script: ScriptPubKey = {
    val t = ScriptPubKey.fromHexT(JBitmexUtxo.getScript)
    if (t.isFailure) {
      println(s"t.failure=${JBitmexUtxo.getScript}")
    }
    t.get
  }
  val balance: Satoshis = Satoshis(JBitmexUtxo.getBalance)
}
