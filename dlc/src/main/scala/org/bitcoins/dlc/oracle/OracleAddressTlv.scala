package org.bitcoins.dlc.oracle

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementTLV,
  OracleAnnouncementV0TLV
}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.StringFactory

import scala.util.{Failure, Success, Try}

trait OracleAddressTlv {
  def hrp: OracleHumanReadablePart

  def oracleAnnouncementTLV: OracleAnnouncementTLV

  private def data: Vector[UInt5] = {
    val u5s = Bech32.from8bitTo5bit(oracleAnnouncementTLV.bytes)
    u5s
  }

  def checksum: Vector[UInt5] = {
    val values = hrp.expand ++ data
    Bech32.createChecksum(values)
  }

  def value: String = {
    val all: Vector[UInt5] = data ++ checksum
    val encoding = Bech32.encode5bitToString(all)

    hrp.toString + Bech32.separator + encoding
  }

  override def toString: String = {
    val b = new StringBuilder
    b.append(hrp.toString)
    b.append(Bech32.separator)

    val dataToString = Bech32.encode5bitToString(data)
    val checksumString = Bech32.encode5bitToString(checksum)

    b.append(dataToString)
    b.append(checksumString)

    b.toString()
  }
}

object OracleAddressTlv extends StringFactory[OracleAddressTlv] {

  private case class OracleAddressTlvImpl(
      hrp: OracleHumanReadablePart,
      oracleAnnouncementTLV: OracleAnnouncementTLV)
      extends OracleAddressTlv

  def apply(
      hrp: OracleHumanReadablePart,
      oracleAnnouncementTLV: OracleAnnouncementTLV): OracleAddressTlv = {
    OracleAddressTlvImpl(hrp, oracleAnnouncementTLV)
  }

  override def fromString(string: String): OracleAddressTlv = {
    fromStringT(string) match {
      case Success(value) => value
      case Failure(err)   => throw err
    }
  }

  override def fromStringT(string: String): Try[OracleAddressTlv] = {
    val sepIndexes = {
      string.zipWithIndex.filter {
        case (sep, _) => sep == Bech32.separator
      }
    }

    if (sepIndexes.isEmpty) {
      throw new IllegalArgumentException(
        "LnInvoice did not have the correct separator")
    } else {
      val (_, sepIndex) = sepIndexes.last

      val hrp = string.take(sepIndex)

      val (_, dataString) = string.splitAt(sepIndex + 1)

      if (hrp.length < 1) {
        throw new IllegalArgumentException("HumanReadablePart is too short")
      } else if (dataString.length < 6) {
        throw new IllegalArgumentException("Data part is too short")
      } else {
        val hrpValid = OracleHumanReadablePart.fromStringT(hrp)

        val dataValid = Bech32.checkDataValidity(dataString)

        val isChecksumValid: Try[Vector[UInt5]] = hrpValid.flatMap {
          case h: OracleHumanReadablePart =>
            dataValid.flatMap { d: Vector[UInt5] =>
              stripChecksumIfValid(h, d)
            }
        }
        val oracleAddressT: Try[OracleAddressTlv] = for {
          d <- isChecksumValid
          h <- hrpValid
          bytes = Bech32.from5bitToByteVector(d)
          announcementTlv = OracleAnnouncementV0TLV.fromBytes(bytes)
        } yield {
          OracleAddressTlv(h, announcementTlv)
        }

        oracleAddressT
      }
    }
  }

  private def stripChecksumIfValid(
      h: OracleHumanReadablePart,
      d: Vector[UInt5]): Try[Vector[UInt5]] = {
    if (verifyChecksum(h, d)) {
      if (d.size < 6) Success(Vector.empty)
      else Success(d.take(d.size - 6))
    } else
      Failure(
        new IllegalArgumentException("Checksum was invalid on the LnInvoice"))
  }

  def verifyChecksum(hrp: OracleHumanReadablePart, u5s: Seq[UInt5]): Boolean = {
    val data = hrp.expand ++ u5s
    val checksum = Bech32.polyMod(data)
    checksum == 1
  }
}
