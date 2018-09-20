package org.bitcoins.core.util

import org.bitcoins.core.number.{ UInt32, UInt8 }
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
 * A abstract class representing basic utility functions of Bech32
 * For more information on Bech32 please seee BIP173
 * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
 */
sealed abstract class Bech32 {

  /** Separator used to separate the hrp & data parts of a bech32 addr */
  val separator = '1'

  /** https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 */
  val charset: Vector[Char] = Vector('q', 'p', 'z', 'r', 'y', '9', 'x', '8',
    'g', 'f', '2', 't', 'v', 'd', 'w', '0',
    's', '3', 'j', 'n', '5', '4', 'k', 'h',
    'c', 'e', '6', 'm', 'u', 'a', '7', 'l')

  private val generators: Vector[Long] = Vector(
    UInt32("3b6a57b2").toLong,
    UInt32("26508e6d").toLong, UInt32("1ea119fa").toLong,
    UInt32("3d4233dd").toLong, UInt32("2a1462b3").toLong)

  private val u32Five = UInt32(5)
  private val u32Eight = UInt32(8)

  /**
   * Creates a checksum for the given byte vector according to BIP173
   * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32]]
   * @param bytes
   * @return
   */
  def createChecksum(bytes: Vector[UInt8]): Vector[UInt8] = {
    val z = UInt8.zero
    val polymod: Long = polyMod(bytes ++ Array(z, z, z, z, z, z)) ^ 1
    //[(polymod >> 5 * (5 - i)) & 31 for i in range(6)]

    val result: Vector[UInt8] = 0.until(6).map { i =>
      //((polymod >> five * (five - u)) & UInt8(31.toShort))
      UInt8(((polymod >> 5 * (5 - i)) & 31).toShort)
    }.toVector
    result
  }

  /**
   * Expands the human readable part of a bech32 address as per BIP173
   * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32]]
   * @param bytes
   * @return
   */
  def hrpExpand(bytes: ByteVector): Vector[UInt8] = {
    val x: ByteVector = bytes.map { b: Byte =>
      (b >> 5).toByte
    }
    val withZero: ByteVector = x ++ ByteVector.low(1)

    val y: ByteVector = bytes.map { char =>
      (char & 0x1f).toByte
    }
    val result = UInt8.toUInt8s(withZero ++ y)
    result
  }

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

  /**
   * Takes in the data portion of a bech32 address and decodes it to a byte array
   * It also checks the validity of the data portion according to BIP173
   */
  def checkDataValidity(data: String): Try[ByteVector] = {
    @tailrec
    def loop(remaining: List[Char], accum: ByteVector, hasUpper: Boolean, hasLower: Boolean): Try[ByteVector] = remaining match {
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
    val payload: Try[ByteVector] = loop(data.toCharArray.toList, ByteVector.empty,
      false, false)
    payload
  }

  /** Takes a base32 byte array and encodes it to a string */
  def encodeToString(b: Vector[UInt8]): String = {
    b.map(b => charset(b.toInt)).mkString
  }

  /** Converts a byte vector from base 8 to base 5 */
  def from8bitTo5bit(bytes: ByteVector): Try[Vector[UInt8]] = {
    val u8s = UInt8.toUInt8s(bytes)
    from8bitTo5bit(u8s)
  }

  /** Converts a byte array from base 8 to base 5 */
  def from8bitTo5bit(bytes: Vector[UInt8]): Try[Vector[UInt8]] = {
    NumberUtil.convertUInt8s(bytes, u32Eight, u32Five, true)
  }

  /** Decodes a byte array from base 5 to base 8 */
  def from5bitTo8bit(b: Vector[UInt8]): Try[Vector[UInt8]] = {
    NumberUtil.convertUInt8s(b, u32Five, u32Eight, false)
  }

}

object Bech32 extends Bech32
