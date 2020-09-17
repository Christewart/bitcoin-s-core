package org.bitcoins.dlc.oracle

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.protocol.ln.{LnTag, LnTaggedFields}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.{SchnorrNonce, SchnorrPublicKey, StringFactory}

import scala.util.{Failure, Success, Try}

trait OracleAddress {
  def hrp: OracleHumanReadablePart

  def data: Vector[UInt5] = {
    publicKey.data ++
      nonce.data ++
      maturation.data ++
      outcomes.flatMap(_.data)
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

  def nonce: LnTag.DLCNonce

  def publicKey: LnTag.DLCPubKey

  def maturation: LnTag.DLCMaturation

  def outcomes: Vector[LnTag.DLCOutcome]

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

object OracleAddress extends StringFactory[OracleAddress] {

  private case class OracleAddressImpl(
      hrp: OracleHumanReadablePart,
      publicKey: LnTag.DLCPubKey,
      nonce: LnTag.DLCNonce,
      maturation: LnTag.DLCMaturation,
      outcomes: Vector[LnTag.DLCOutcome])
      extends OracleAddress

  val separator: Char = Bech32.separator

  def apply(
      hrp: OracleHumanReadablePart,
      publicKey: SchnorrPublicKey,
      nonce: SchnorrNonce,
      maturation: LnTag.DLCMaturation,
      outcomes: Vector[String]): OracleAddress = {
    OracleAddress(hrp = hrp,
                  publicKey = LnTag.DLCPubKey(publicKey),
                  nonce = LnTag.DLCNonce(nonce),
                  maturation = maturation,
                  outcomes = outcomes.map(LnTag.DLCOutcome(_)))
  }

  def apply(
      hrp: OracleHumanReadablePart,
      publicKey: LnTag.DLCPubKey,
      nonce: LnTag.DLCNonce,
      maturation: LnTag.DLCMaturation,
      outcomes: Vector[LnTag.DLCOutcome]): OracleAddress = {
    OracleAddressImpl(hrp = hrp,
                      publicKey = publicKey,
                      nonce = nonce,
                      maturation = maturation,
                      outcomes = outcomes)
  }

  override def fromString(string: String): OracleAddress = {
    fromStringT(string) match {
      case Success(value) => value
      case Failure(err)   => throw err
    }
  }

  override def fromStringT(string: String): Try[OracleAddress] = {
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
        val oracleAddressT: Try[OracleAddress] = isChecksumValid.flatMap {
          d: Vector[UInt5] =>
            val taggedFields = LnTaggedFields.fromUInt5s(d)
            val pubKey = taggedFields.collectFirst {
              case x: LnTag.DLCPubKey => x
            }.get

            val nonce = taggedFields.collectFirst {
              case x: LnTag.DLCNonce => x
            }.get

            val outcomes = taggedFields.collect {
              case o: LnTag.DLCOutcome => o
            }

            val maturation = taggedFields.collectFirst {
              case m: LnTag.DLCMaturation => m
            }.get

            hrpValid.map { hrp =>
              OracleAddress(hrp, pubKey, nonce, maturation, outcomes.toVector)
            }

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
