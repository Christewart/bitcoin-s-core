package org.bitcoins.scripts.bitmex

case class BitmexLiabilities(
    blockHeight: Long,
    liabilities: Vector[BitmexUserLiability]) {

  override def toString: String = {
    s"BitmexLiabilities($blockHeight, liabilityCount=${liabilities.length})"
  }
}
