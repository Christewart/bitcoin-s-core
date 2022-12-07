package org.bitcoins.scripts.bitmex

case class BitmexClaim(JBitmexClaim: JBitmexClaim) {
  val m: Long = JBitmexClaim.getM
  val n: Long = JBitmexClaim.getN

  override def toString: String = {
    s"BitmexClaim(m=$m,n=$n)"
  }
}
