package org.bitcoins.core.crypto

import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 3/23/16.
  */
sealed abstract class DERSignatureUtil {

  /**
    * Checks if this signature is encoded to DER correctly
    * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
    * NOTE: This will fail if this signature contains the hash type appended to the end of it
    * @return boolean representing if the signature is a valid
    */
  def isDEREncoded(signature: ECDigitalSignature): Boolean =
    isDEREncoded(signature.bytes)

  /**
    * Checks if the bytes are encoded to DER correctly
    * https://crypto.stackexchange.com/questions/1795/how-can-i-convert-a-der-ecdsa-signature-to-asn-1
    * @return boolean representing if the signature is a valid
    */
  def isDEREncoded(
      bytes: ByteVector,
      withSighashByte: Boolean = false,
      withStrictMode: Boolean = false): Boolean = {
    // Format: 0x30 [total-length] 0x02 [R-length] [R] 0x02 [S-length] [S]
    // min (non-zero) length is 8 bytes, 1 byte for R and S
    // max length is 72 bytes, 33 bytes for R and S
    val minLen = if (withSighashByte) 9 else 8
    val maxLen = if (withSighashByte) 73 else 72
    val maxIntLen = 33
    val seqTag = 0x30
    val lenWithoutSeqContents = bytes.size - (if (withSighashByte) 3 else 2)
    val intTag = 0x02

    if (bytes.isEmpty) true
    else if (bytes.size < minLen)
      false // must be within size bounds
    else if (withStrictMode && bytes.length > maxLen)
      false
    else if (bytes(0) != seqTag)
      false // expected ASN.1 DER ‘Constructed’ SEQUENCE
    else if (if (withStrictMode) bytes(1) != lenWithoutSeqContents else bytes(1) < lenWithoutSeqContents) false // invalid SEQUENCE length
    else if (bytes(2) != intTag) false // expected ASN.1 INTEGER
    else {
      val rLen = bytes(3)
      val minLenWithoutMinR = minLen - 1

      if (rLen < 1 || rLen > maxIntLen || (minLenWithoutMinR + rLen) > bytes.size)
        false // R length must be [1,33] bytes and not overrun
      else {
        val sTagOffset = 4 + rLen

        if (bytes(sTagOffset) != intTag) false // expected ASN.1 INTEGER
        else {
          val sLenOffset = sTagOffset + 1
          val sLen = bytes(sLenOffset)
          val constantSize = if (withSighashByte) 7 else 6

          if (sLen < 1 || sLen > maxIntLen)
            false // S length must be [1,33] bytes
          else if (if (withStrictMode) (constantSize + rLen + sLen) != bytes.size else (constantSize + rLen + sLen) > bytes.size)
            false // S length must not overrun
          else {
            if (withStrictMode) {
              val rOffset = 4
              if (rLen > 1 && bytes(rOffset) == 0 && (bytes(rOffset + 1) & 0x80) == 0)
                false // no null padding at start of R
              else if ((bytes(rOffset) & 0x80) != 0)
                false // no negative numbers for R
              else {
                val sOffset = sLenOffset + 1

                if (sLen > 1 && bytes(sOffset) == 0 && (bytes(sOffset + 1) & 0x80) == 0)
                  false // no null padding at start of S
                else if ((bytes(sOffset) & 0x80) != 0)
                  false // no negative numbers for S
                else true
              }
            } else true
          }
        }
      }
    }
  }

  /**
    * Decodes the given digital signature into it's r and s points
    * @param signature
    * @return
    */
  def decodeSignature(signature: ECDigitalSignature): (BigInt, BigInt) =
    decodeSignature(signature.bytes)

  /**
    * Decodes the given sequence of bytes into it's r and s points
    * throws an exception if the given sequence of bytes is not a DER encoded signature
    * @param bytes
    * @return
    */
  def decodeSignature(
      bytes: ByteVector,
      withStrictMode: Boolean = false): (BigInt, BigInt) = {

    def hexStrForByte(ix: Int): String = {
      s"0x${java.lang.Integer.toHexString(java.lang.Byte.toUnsignedInt(bytes(ix)))}"
    }

    val minLen = 8 // if (withSighashByte) 9 else 8
    val maxLen = 73 // if (withSighashByte) 73 else 72
    val maxIntLen = 33
    val seqTag = 0x30
    val intTag = 0x02

    if (bytes.isEmpty) (BigInt(0), BigInt(0))
    else if (bytes.size < minLen) {
      throw new IllegalArgumentException(
        s"Expected at least ${minLen} bytes, was: ${bytes.length}")
    } else if (withStrictMode && bytes.length > maxLen) {
      throw new IllegalArgumentException(
        s"Expected at most ${maxLen} bytes, was: ${bytes.length}")
    } else if (bytes(0) != seqTag) {
      throw new IllegalArgumentException(
        s"Expected ASN.1 DER ‘Constructed’ SEQUENCE (0x30), was: ${hexStrForByte(0)}")
    } else {
      val seqLen = bytes(1)

      if (bytes.size < seqLen + 2) {
        throw new IllegalArgumentException(
          s"Invalid ASN.1 SEQUENCE length, was: ${hexStrForByte(1)}")
      } else if (withStrictMode && bytes.size > seqLen + 3) {
        throw new IllegalArgumentException(
          s"Invalid ASN.1 SEQUENCE length, was: ${hexStrForByte(1)}")
      } else {
        val withSighashByte = bytes.size == seqLen + 3

        if (bytes(2) != intTag) {
          throw new IllegalArgumentException(
            s"Expected ASN.1 INTEGER (0x02), was: ${hexStrForByte(2)}")
        } else {
          val rLen = bytes(3)
          val minLenWithoutMinR = minLen - 1

          if (rLen < 1 || rLen > maxIntLen || (minLenWithoutMinR + rLen) > bytes.size) {
            throw new IllegalArgumentException(
              s"Length of ASN.1 INTEGER for R must be [1,33] bytes and not overrun, was: ${rLen}")
          } else {
            val sTagOffset = 4 + rLen

            if (bytes(sTagOffset) != intTag) {
              throw new IllegalArgumentException(
                s"Expected ASN.1 INTEGER (0x02), was: ${hexStrForByte(sTagOffset)}")
            } else {
              val sLenOffset = sTagOffset + 1
              val sLen = bytes(sLenOffset)
              val constantSize = if (withSighashByte) 7 else 6

              if (sLen < 1 || sLen > maxIntLen) {
                throw new IllegalArgumentException(
                  s"Length of ASN.1 INTEGER for S must be [1,33] bytes, was: ${sLen}")
              } else if (if (withStrictMode) (constantSize + rLen + sLen) != bytes.size else (constantSize + rLen + sLen) > bytes.size) {
                throw new IllegalArgumentException(
                  s"Length of ASN.1 INTEGER for S must not overrun, was: ${sLen}")
              } else {
                val rOffset = 4
                val sOffset = sLenOffset + 1
                def extractRS = {
                  // force positive integers, even if ASN.1 integer was technically negative
                  // this has an impact only when strict mode is turned off, as negative ASN.1 Integers are rejected
                  val r =
                    BigInt(1, bytes.slice(rOffset, rOffset + rLen).toArray)
                  val s =
                    BigInt(1, bytes.slice(sOffset, sOffset + sLen).toArray)
                  (r, s)
                }

                if (withStrictMode) {
                  if (rLen > 1 && bytes(rOffset) == 0 && (bytes(rOffset + 1) & 0x80) == 0) {
                    throw new IllegalArgumentException(
                      s"ASN.1 INTEGER for R has a leading null byte")
                  } else if ((bytes(rOffset) & 0x80) != 0) {
                    throw new IllegalArgumentException(
                      s"R value in ECDSA must be non-negative in strict mode")
                  } else if (sLen > 1 && bytes(sOffset) == 0 && (bytes(
                               sOffset + 1) & 0x80) == 0) {
                    throw new IllegalArgumentException(
                      s"ASN.1 INTEGER for S has a leading null byte")
                  } else if ((bytes(sOffset) & 0x80) != 0) {
                    throw new IllegalArgumentException(
                      s"S value in ECDSA must be non-negative in strict mode")
                  } else extractRS
                } else extractRS
              }
            }
          }
        }
      }
    }
  }

  /**
    * This functions implements the strict der encoding rules that were created in BIP66
    * [[https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki]]
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L98]]
    * @param signature the signature to check if they are strictly der encoded
    * @return boolean indicating whether the signature was der encoded or not
    */
  def isValidSignatureEncoding(signature: ECDigitalSignature): Boolean = {
    signature match {
      case EmptyDigitalSignature => true
      case signature: ECDigitalSignature =>
        isValidSignatureEncoding(signature.bytes)
    }
  }

  /**
    * This functions implements the strict der encoding rules that were created in BIP66
    * https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L98]]
    * @param bytes the bytes to check if they are strictly der encoded
    * @return boolean indicating whether the bytes were der encoded or not
    */
  def isValidSignatureEncoding(bytes: ByteVector): Boolean = {
    isDEREncoded(bytes, withSighashByte = true, withStrictMode = true)
  }

  /**
    * Requires the S value in signatures to be the low version of the S value
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @param signature
    * @return if the S value is the low version
    */
  def isLowS(signature: ECDigitalSignature): Boolean = isLowS(signature.bytes)

  /**
    * Requires the S value in signatures to be the low version of the S value
    * https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#low-s-values-in-signatures
    * @param signature
    * @return if the S value is the low version
    */
  def isLowS(signature: ByteVector): Boolean = {
    val result = Try {
      val (_, s) = decodeSignature(signature)
      s.bigInteger.compareTo(CryptoParams.halfCurveOrder) <= 0
    }
    result match {
      case Success(bool) => bool
      case Failure(_)    => false
    }
  }

  /** Checks if the given digital signature uses a low s value, if it does not it converts it to a low s value and returns it */
  def lowS(signature: ECDigitalSignature): ECDigitalSignature = {
    val sigLowS =
      if (isLowS(signature)) signature
      else
        ECDigitalSignature(
          signature.r,
          CryptoParams.curve.getN().subtract(signature.s.bigInteger))
    require(DERSignatureUtil.isLowS(sigLowS))
    sigLowS
  }
}

object DERSignatureUtil extends DERSignatureUtil
