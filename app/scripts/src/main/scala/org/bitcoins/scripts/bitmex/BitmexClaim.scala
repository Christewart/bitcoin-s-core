package org.bitcoins.scripts.bitmex

case class BitmexClaim(JBitmexClaim: JBitmexClaim) {
  val m: Long = JBitmexClaim.getM
  val n: Long = JBitmexClaim.getN
}
