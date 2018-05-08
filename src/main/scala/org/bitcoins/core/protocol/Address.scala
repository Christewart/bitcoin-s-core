package org.bitcoins.core.protocol
import org.bitcoins.core.config._
import org.bitcoins.core.config.{ MainNet, RegTest, TestNet3 }
import org.bitcoins.core.crypto.{ ECPublicKey, HashDigest, Sha256Digest, Sha256Hash160Digest }
import org.bitcoins.core.number.{ UInt32, UInt8 }
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

sealed abstract class Address {

  /** The network that this address is valid for */
  def networkParameters: NetworkParameters

  /** The string representation of this address */
  def value: String

  /** Every address is derived from a [[HashDigest]] in a [[TransactionOutput]] */
  def hash: HashDigest

  /** The [[ScriptPubKey]] the address represents */
  def scriptPubKey: ScriptPubKey
}

sealed abstract class BitcoinAddress extends Address

sealed abstract class P2PKHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value: String = {
    val versionByte = networkParameters.p2pkhNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def hash: Sha256Hash160Digest

  override def scriptPubKey: P2PKHScriptPubKey = P2PKHScriptPubKey(hash)

}

sealed abstract class P2SHAddress extends BitcoinAddress {
  /** The base58 string representation of this address */
  override def value: String = {
    val versionByte = networkParameters.p2shNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def scriptPubKey = P2SHScriptPubKey(hash)

  override def hash: Sha256Hash160Digest
}

/**
 * https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
 */
sealed abstract class Bech32Address extends BitcoinAddress {

  private def logger = BitcoinSLogger.logger

  def hrp: HumanReadablePart

  def data: Seq[UInt8]

  override def networkParameters = hrp.network.get

  override def value: String = {
    val checksum = Bech32Address.createChecksum(hrp, data)
    val all = data ++ checksum
    val encoding = Bech32Address.encodeToString(all)
    hrp.toString + Bech32Address.separator + encoding
  }

  override def scriptPubKey: WitnessScriptPubKey = {
    Bech32Address.fromStringToWitSPK(value).get
  }

  override def hash: Sha256Digest = Sha256Digest(scriptPubKey.witnessProgram.flatMap(_.bytes))

  override def toString = "Bech32Address(" + value + ")"

}

object Bech32Address {
  private case class Bech32AddressImpl(hrp: HumanReadablePart, data: Seq[UInt8]) extends Bech32Address {
    verifyChecksum(hrp, UInt8.toBytes(data))
  }

  /** Separator used to separate the hrp & data parts of a bech32 addr */
  val separator = '1'

  private val logger = BitcoinSLogger.logger

  def apply(
    witSPK: WitnessScriptPubKey,
    networkParameters: NetworkParameters): Try[Bech32Address] = {
    //we don't encode the wit version or pushop for program into base5
    val prog = UInt8.toUInt8s(witSPK.asmBytes.tail.tail)
    val encoded = Bech32Address.encode(prog)
    val hrpTry: Try[HumanReadablePart] = networkParameters match {
      case _: MainNet => Success(bc)
      case _: TestNet3 | _: RegTest => Success(tb)
      case _: ZCashMainNet | _: ZCashTestNet | _: ZCashRegTest =>
        Failure(new IllegalArgumentException("Cannot create a bech32 address from a zcash network"))
    }
    val witVersion = witSPK.witnessVersion.version.toLong.toShort
    hrpTry.flatMap { hrp =>
      encoded.map(e => Bech32Address(hrp, Seq(UInt8(witVersion)) ++ e))
    }
  }

  def apply(hrp: HumanReadablePart, data: Seq[UInt8]): Bech32Address = {
    Bech32AddressImpl(hrp, data)
  }

  /** Returns a base 5 checksum as specified by BIP173 */
  def createChecksum(hrp: HumanReadablePart, bytes: Seq[UInt8]): Seq[UInt8] = {
    val values: Seq[UInt8] = hrpExpand(hrp) ++ bytes
    val z = UInt8.zero
    val polymod: Long = polyMod(values ++ Seq(z, z, z, z, z, z)) ^ 1
    //[(polymod >> 5 * (5 - i)) & 31 for i in range(6)]
    val result: Seq[UInt8] = 0.until(6).map { i =>
      val u = UInt8(i.toShort)
      val five = UInt8(5.toShort)
      //((polymod >> five * (five - u)) & UInt8(31.toShort))
      UInt8(((polymod >> 5 * (5 - i)) & 31).toShort)
    }
    result
  }

  def hrpExpand(hrp: HumanReadablePart): Seq[UInt8] = {
    val x: Seq[Byte] = hrp.bytes.map { b: Byte =>
      (b >> 5).toByte
    }
    val withZero: Seq[Byte] = x ++ Seq(0.toByte)

    val y: Seq[Byte] = hrp.bytes.map { char =>
      (char & 0x1f).toByte
    }
    val result = UInt8.toUInt8s(withZero ++ y)
    result
  }

  private def generators: Seq[Long] = Seq(
    UInt32("3b6a57b2").toLong,
    UInt32("26508e6d").toLong, UInt32("1ea119fa").toLong,
    UInt32("3d4233dd").toLong, UInt32("2a1462b3").toLong)

  def polyMod(bytes: Seq[UInt8]): Long = {
    var chk: Long = 1
    bytes.map { v =>
      val b = chk >> 25
      //chk = (chk & 0x1ffffff) << 5 ^ v
      chk = (chk & 0x1ffffff) << 5 ^ v.toLong
      0.until(5).map { i: Int =>
        //chk ^= GEN[i] if ((b >> i) & 1) else 0
        if (((b >> i) & 1) == 1) {
          chk = chk ^ generators(i)
        }
      }
    }
    chk
  }

  def verifyChecksum(hrp: HumanReadablePart, data: Seq[Byte]): Boolean = {
    val u8s = UInt8.toUInt8s(data)
    verifyCheckSum(hrp, u8s)
  }

  def verifyCheckSum(hrp: HumanReadablePart, u8s: Seq[UInt8]): Boolean = {
    polyMod(hrpExpand(hrp) ++ u8s) == 1
  }

  private val u32Five = UInt32(5)
  private val u32Eight = UInt32(8)

  /** Converts a byte array from base 8 to base 5 */
  def encode(bytes: Seq[UInt8]): Try[Seq[UInt8]] = {
    NumberUtil.convertUInt8s(bytes, u32Eight, u32Five, true)
  }
  /** Decodes a byte array from base 5 to base 8 */
  def decodeToBase8(b: Seq[UInt8]): Try[Seq[UInt8]] = {
    NumberUtil.convertUInt8s(b, u32Five, u32Eight, false)
  }

  /** Tries to convert the given string a to a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]] */
  def fromStringToWitSPK(string: String): Try[WitnessScriptPubKey] = {
    val decoded = fromString(string)
    decoded.flatMap {
      case bec32Addr =>
        val bytes = UInt8.toBytes(bec32Addr.data)
        val (v, prog) = (bytes.head, bytes.tail)
        val convertedProg = NumberUtil.convertBytes(prog, u32Five, u32Eight, false)
        val progBytes = convertedProg.map(UInt8.toBytes(_))
        val witVersion = WitnessVersion(v)
        progBytes.flatMap { prog =>
          val pushOp = BitcoinScriptUtil.calculatePushOp(prog)
          witVersion match {
            case Some(v) =>
              WitnessScriptPubKey(Seq(v.version) ++ pushOp ++ Seq(ScriptConstant(prog))) match {
                case Some(spk) => Success(spk)
                case None => Failure(new IllegalArgumentException("Failed to decode bech32 into a witSPK"))
              }
            case None => Failure(new IllegalArgumentException("Witness version was not valid, got: " + v))
          }

        }
    }
  }
  /** Takes a base32 byte array and encodes it to a string */
  def encodeToString(b: Seq[UInt8]): String = {
    b.map(b => charset(b.toInt)).mkString
  }
  /** Decodes bech32 string to the [[HumanReadablePart]] & data part */
  def fromString(str: String): Try[Bech32Address] = {
    val sepIndexes = str.zipWithIndex.filter(_._1 == separator)
    if (str.size > 90 || str.size < 8) {
      Failure(new IllegalArgumentException("bech32 payloads must be betwee 8 and 90 chars, got: " + str.size))
    } else if (sepIndexes.isEmpty) {
      Failure(new IllegalArgumentException("Bech32 address did not have the correct separator"))
    } else {
      val sepIndex = sepIndexes.last._2
      val (hrp, data) = (str.take(sepIndex), str.splitAt(sepIndex + 1)._2)
      if (hrp.size < 1 || data.size < 6) {
        Failure(new IllegalArgumentException("Hrp/data too short"))
      } else {
        val hrpValid = checkHrpValidity(hrp)
        val dataValid = checkDataValidity(data)
        val isChecksumValid: Try[Seq[Byte]] = hrpValid.flatMap { h =>
          dataValid.flatMap { d =>
            if (verifyChecksum(h, d)) {
              if (d.size < 6) Success(Nil)
              else Success(d.take(d.size - 6))
            } else Failure(new IllegalArgumentException("Checksum was invalid on the bech32 address"))
          }
        }
        isChecksumValid.flatMap { d: Seq[Byte] =>
          val u8s = UInt8.toUInt8s(d)
          hrpValid.map(h => Bech32Address(h, u8s))
        }
      }
    }
  }

  /** Checks if the possible human readable part follows BIP173 rules */
  private def checkHrpValidity(hrp: String): Try[HumanReadablePart] = {
    @tailrec
    def loop(remaining: List[Char], accum: Seq[UInt8], isLower: Boolean, isUpper: Boolean): Try[Seq[UInt8]] = remaining match {
      case h :: t =>
        if (h < 33 || h > 126) {
          Failure(new IllegalArgumentException("Invalid character range for hrp, got: " + hrp))
        } else if (isLower && isUpper) {
          Failure(new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          loop(t, UInt8(h.toByte) +: accum, h.isLower || isLower, h.isUpper || isUpper)
        }
      case Nil =>
        if (isLower && isUpper) {
          Failure(new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          Success(accum.reverse)
        }
    }

    loop(hrp.toCharArray.toList, Nil, false, false).flatMap { _ =>
      Success(HumanReadablePart(hrp.toLowerCase))
    }
  }

  /**
   * Takes in the data portion of a bech32 address and decodes it to a byte array
   * It also checks the validity of the data portion according to BIP173
   */
  def checkDataValidity(data: String): Try[Seq[Byte]] = {
    @tailrec
    def loop(remaining: List[Char], accum: Seq[Byte], hasUpper: Boolean, hasLower: Boolean): Try[Seq[Byte]] = remaining match {
      case Nil => Success(accum.reverse)
      case h :: t =>
        if (!charset.contains(h.toLower)) {
          Failure(new IllegalArgumentException("Invalid character in data of bech32 address, got: " + h))
        } else {
          if ((h.isUpper && hasLower) || (h.isLower && hasUpper)) {
            Failure(new IllegalArgumentException("Cannot have mixed case for bech32 address"))
          } else {
            val byte = charset.indexOf(h.toLower).toByte
            require(byte >= 0 && byte < 32, "Not in valid range, got: " + byte)
            loop(t, byte +: accum, h.isUpper || hasUpper, h.isLower || hasLower)
          }
        }
    }
    val payload: Try[Seq[Byte]] = loop(data.toCharArray.toList, Nil, false, false)
    payload
  }

  /** https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 */
  def charset: Seq[Char] = Seq('q', 'p', 'z', 'r', 'y', '9', 'x', '8',
    'g', 'f', '2', 't', 'v', 'd', 'w', '0',
    's', '3', 'j', 'n', '5', '4', 'k', 'h',
    'c', 'e', '6', 'm', 'u', 'a', '7', 'l')
}

object P2PKHAddress {
  private case class P2PKHAddressImpl(
    hash: Sha256Hash160Digest,
    networkParameters: NetworkParameters) extends P2PKHAddress

  def apply(hash: Sha256Hash160Digest, network: NetworkParameters): P2PKHAddress = P2PKHAddressImpl(hash, network)

  def apply(pubKey: ECPublicKey, networkParameters: NetworkParameters): P2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    P2PKHAddress(hash, networkParameters)
  }

  def apply(spk: P2PKHScriptPubKey, networkParameters: NetworkParameters): P2PKHAddress = {
    P2PKHAddress(spk.pubKeyHash, networkParameters)
  }

  /** Checks if an address is a valid p2pkh address */
  def isP2PKHAddress(address: String): Boolean = Try(fromString(address, Networks.knownNetworks)).isSuccess

  def fromString(address: String, validNetworks: Seq[NetworkParameters]): P2PKHAddress = {
    val decodeCheckP2PKH: Try[Seq[Byte]] = Base58.decodeCheck(address)
    decodeCheckP2PKH match {
      case Success(bytes) =>
        val networkBytes: Option[(NetworkParameters, Seq[Byte])] = validNetworks.map(n => (n, n.p2pkhNetworkByte))
          .find {
            case (_, bs) =>
              bytes.startsWith(bs)
          }
        val result: Option[P2PKHAddress] = networkBytes.map {
          case (network, p2pkhNetworkBytes) =>
            val payloadSize = bytes.size - p2pkhNetworkBytes.size
            require(payloadSize == 20, s"Payload of a P2PKH address must be 20 bytes in size, got $payloadSize")
            val payload = bytes.slice(p2pkhNetworkBytes.size, bytes.size)
            P2PKHAddress(Sha256Hash160Digest(payload), network)
        }

        result.getOrElse(throw new IllegalArgumentException(s"Given address was not a valid P2PKH address, got: $address"))
      case Failure(exception) =>
        throw new IllegalArgumentException(s"Given address was not a valid P2PKH address, got: $address")
    }
  }

  @deprecated("You need to provide a network to determine if a p2sh address is valid", "2018/05/07")
  def fromString(address: String): P2PKHAddress = fromString(address, Networks.knownNetworks)

  /** Checks if an address is a valid p2pkh address */
  def isP2PKHAddress(address: BitcoinAddress): Boolean = isP2PKHAddress(address.value)

}

object P2SHAddress {
  private case class P2SHAddressImpl(
    hash: Sha256Hash160Digest,
    networkParameters: NetworkParameters) extends P2SHAddress

  /**
   * Creates a [[P2SHScriptPubKey]] from the given [[ScriptPubKey]],
   * then creates an address from that [[P2SHScriptPubKey]]
   */
  def apply(scriptPubKey: ScriptPubKey, network: NetworkParameters): P2SHAddress = {
    val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
    P2SHAddress(p2shScriptPubKey, network)
  }

  def apply(p2shScriptPubKey: P2SHScriptPubKey, network: NetworkParameters): P2SHAddress = P2SHAddress(p2shScriptPubKey.scriptHash, network)

  def apply(hash: Sha256Hash160Digest, network: NetworkParameters): P2SHAddress = P2SHAddressImpl(hash, network)

  /** Checks if a address is a valid p2sh address */
  def isP2SHAddress(address: String): Boolean = Try(fromString(address, Networks.knownNetworks)).isSuccess

  /** Checks if a address is a valid p2sh address */
  def isP2SHAddress(address: BitcoinAddress): Boolean = isP2SHAddress(address.value)

  def fromString(address: String, validNetworks: Seq[NetworkParameters]): P2SHAddress = {
    val decodeCheckP2PKH: Try[Seq[Byte]] = Base58.decodeCheck(address)
    decodeCheckP2PKH match {
      case Success(bytes) =>
        val networkBytes: Option[(NetworkParameters, Seq[Byte])] = validNetworks.map(n => (n, n.p2shNetworkByte))
          .find {
            case (_, bs) =>
              bytes.startsWith(bs)
          }
        val result: Option[P2SHAddress] = networkBytes.map {
          case (network, p2shNetworkBytes) =>
            val payloadSize = bytes.size - p2shNetworkBytes.size
            require(payloadSize == 20, s"Payload of a P2PKH address must be 20 bytes in size, got $payloadSize")
            val payload = bytes.slice(p2shNetworkBytes.size, bytes.size)
            P2SHAddress(Sha256Hash160Digest(payload), network)
        }
        result.getOrElse(throw new IllegalArgumentException(s"Given address was not a valid P2PKH address, got: $address"))
      case Failure(exception) =>
        throw new IllegalArgumentException(s"Given address was not a valid P2PKH address, got: $address")
    }
  }

  @deprecated("You need to provide a network to determine if a p2sh address is valid", "2018/05/07")
  def fromString(address: String): P2SHAddress = fromString(address, Networks.knownNetworks)

}

object BitcoinAddress {
  private val logger = BitcoinSLogger.logger
  /** Checks if the given base58 bitcoin address is a valid address */
  def validate(bitcoinAddress: String): Boolean = {
    val decodeChecked = Base58.decodeCheck(bitcoinAddress)
    decodeChecked.isSuccess
  }

  /** Creates a [[BitcoinAddress]] from the given string value */
  def apply(value: String): BitcoinAddress = fromString(value)

  def fromString(value: String): BitcoinAddress = {
    val p2pkhTry = Try(P2PKHAddress.fromString(value, BitcoinNetworks.knownNetworks))
    if (p2pkhTry.isSuccess) {
      p2pkhTry.get
    } else {
      val p2shTry = Try(P2SHAddress.fromString(value, BitcoinNetworks.knownNetworks))
      if (p2shTry.isSuccess) {
        p2shTry.get
      } else {
        val bech32Try = Bech32Address.fromString(value)
        if (bech32Try.isSuccess) {
          bech32Try.get
        } else {
          throw new IllegalArgumentException(s"Could not decode the given value to a BitcoinAddress, got: $value")
        }
      }
    }
  }

  /** Helper function for helping matching an address to a network byte */
  private def matchNetwork(bytes: Seq[Byte]): Option[(NetworkParameters, Seq[Byte])] = {
    val all: Seq[Seq[Byte]] = Networks.p2pkhNetworkBytes ++ Networks.p2shNetworkBytes
    val hex = all.map(BitcoinSUtil.encodeHex(_))
    val networkByte = all.find { b =>
      bytes.startsWith(b)
    }
    val payload = networkByte.map(b => bytes.splitAt(b.size)._2)
    val result: Option[(NetworkParameters, Seq[Byte])] = networkByte.flatMap { nb =>
      payload.map { p =>
        (Networks.bytesToNetwork(nb), p)
      }
    }
    result
  }
}

object Address {

  def fromBytes(bytes: Seq[Byte]): Try[Address] = {
    val encoded = Base58.encode(bytes)
    Try(BitcoinAddress(encoded))
  }

  def fromHex(hex: String): Try[Address] = fromBytes(BitcoinSUtil.decodeHex(hex))

  def apply(bytes: Seq[Byte]): Try[Address] = fromBytes(bytes)

  def apply(str: String): Try[Address] = fromString(str)

  def fromString(str: String): Try[Address] = {
    Try(BitcoinAddress(str))
  }
  def fromScriptPubKey(spk: ScriptPubKey, network: NetworkParameters): Try[BitcoinAddress] = spk match {
    case p2pkh: P2PKHScriptPubKey => Success(P2PKHAddress(p2pkh, network))
    case p2sh: P2SHScriptPubKey => Success(P2SHAddress(p2sh, network))
    case witSPK: WitnessScriptPubKey => Bech32Address(witSPK, network)
    case x @ (_: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: LockTimeScriptPubKey
      | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey
      | _: WitnessCommitment | _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
      Failure(new IllegalArgumentException("Cannot create a address for the scriptPubKey: " + x))
  }

  def apply(spk: ScriptPubKey, networkParameters: NetworkParameters): Try[BitcoinAddress] = {
    fromScriptPubKey(spk, networkParameters)
  }
}

sealed abstract class ZcashAddress extends Address {
  override def networkParameters: ZCashNetwork
}

sealed abstract class ZcashP2PKHAddress extends ZcashAddress {

  override def value: String = {
    val versionByte = networkParameters.p2pkhNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def hash: Sha256Hash160Digest
  override def scriptPubKey: P2PKHScriptPubKey = P2PKHScriptPubKey(hash)

}

object ZcashP2PKHAddress {
  private case class ZcashP2PKHAddressImpl(
    hash: Sha256Hash160Digest,
    networkParameters: ZCashNetwork) extends ZcashP2PKHAddress

  def apply(pubKey: ECPublicKey, networkParameters: ZCashNetwork): ZcashP2PKHAddress = {
    val hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    ZcashP2PKHAddress(hash, networkParameters)
  }

  def apply(spk: P2PKHScriptPubKey, networkParameters: ZCashNetwork): ZcashP2PKHAddress = {
    ZcashP2PKHAddress(spk.pubKeyHash, networkParameters)
  }

  def apply(hash: Sha256Hash160Digest, networkParameters: ZCashNetwork): ZcashP2PKHAddress = {
    ZcashP2PKHAddressImpl(hash, networkParameters)
  }

  def fromString(str: String): ZcashP2PKHAddress = {
    val p2pkhAddr = P2PKHAddress.fromString(str, ZCashNetworks.knownNetworks)
    ZcashP2PKHAddress(p2pkhAddr.hash, p2pkhAddr.networkParameters.asInstanceOf[ZCashNetwork])
  }
}

sealed abstract class ZcashP2SHAddress extends ZcashAddress {
  /** The base58 string representation of this address */
  override def value: String = {
    val versionByte = networkParameters.p2shNetworkByte
    val bytes = versionByte ++ hash.bytes
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  override def scriptPubKey = P2SHScriptPubKey(hash)

  override def hash: Sha256Hash160Digest
}

object ZcashP2SHAddress {
  private case class ZcashP2SHAddressImpl(hash: Sha256Hash160Digest, networkParameters: ZCashNetwork) extends ZcashP2SHAddress

  def apply(hash: Sha256Hash160Digest, networkParameters: ZCashNetwork): ZcashP2SHAddress = {
    ZcashP2SHAddressImpl(hash, networkParameters)
  }

  def apply(scriptPubKey: ScriptPubKey, network: ZCashNetwork): ZcashP2SHAddress = {
    val p2shScriptPubKey = P2SHScriptPubKey(scriptPubKey)
    ZcashP2SHAddress(p2shScriptPubKey, network)
  }

  def apply(p2shScriptPubKey: P2SHScriptPubKey, network: ZCashNetwork): ZcashP2SHAddress = {
    ZcashP2SHAddress(p2shScriptPubKey.scriptHash, network)
  }

  def fromString(str: String): ZcashP2SHAddress = {
    val p2shAddr = P2SHAddress.fromString(str, ZCashNetworks.knownNetworks)
    ZcashP2SHAddress(p2shAddr.hash, p2shAddr.networkParameters.asInstanceOf[ZCashNetwork])
  }
}

object ZcashAddress {
  def isValid(zcashAddr: String): Boolean = {
    val decodeChecked = Base58.decodeCheck(zcashAddr)
    decodeChecked.isSuccess
  }

  def fromString(value: String): ZcashAddress = {
    val p2pkhTry = Try(ZcashP2PKHAddress.fromString(value))
    if (p2pkhTry.isSuccess) {
      p2pkhTry.get
    } else {
      val p2shTry = Try(ZcashP2SHAddress.fromString(value))
      if (p2shTry.isSuccess) {
        p2shTry.get
      } else {
        throw new IllegalArgumentException(s"Could not decode the given value to a ZcashAddress, got: $value")
      }
    }
  }
}