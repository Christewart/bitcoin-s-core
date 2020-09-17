package org.bitcoins.dlc.oracle

import org.bitcoins.core.util.Bech32HumanReadablePart
import org.bitcoins.crypto.StringFactory

case class OracleHumanReadablePart() extends Bech32HumanReadablePart {
  override lazy val chars: String = OracleHumanReadablePart.chars

  override def toString: String = chars
}

object OracleHumanReadablePart extends StringFactory[OracleHumanReadablePart] {
  val chars: String = "oracle"

  val hrp: OracleHumanReadablePart = OracleHumanReadablePart()

  override def fromString(string: String): OracleHumanReadablePart = {
    string match {
      case OracleHumanReadablePart.chars => hrp
      case x =>
        sys.error(s"Cannot create oracle human readable part from string=${x} ")
    }
  }
}
