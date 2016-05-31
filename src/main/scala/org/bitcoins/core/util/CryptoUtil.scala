package org.bitcoins.core.util

import java.security.MessageDigest

import org.bitcoinj.core.Sha256Hash
import org.bitcoins.core.crypto._
import org.spongycastle.crypto.digests.RIPEMD160Digest

/**
 * Created by chris on 1/14/16.
 * Utility cryptographic functions
 */
trait CryptoUtil {

  /**
   * Does the following computation
   * RIPEMD160(SHA256(hex))
 *
   * @param hex
   * @return
   */
  def sha256Hash160(hex : String) : Sha256Hash160Digest = sha256Hash160(BitcoinSUtil.decodeHex(hex))

  def sha256Hash160(bytes : List[Byte]) : Sha256Hash160Digest = {
    val hash = org.bitcoinj.core.Utils.sha256hash160(bytes.toArray)
    Sha256Hash160Digest(hash.toList)
  }
  /**
   * Performs sha256(sha256(hex))
 *
   * @param hex
   * @return
   */
  def doubleSHA256(hex : String) : DoubleSha256Digest = doubleSHA256(BitcoinSUtil.decodeHex(hex))
  /**
   * Performs sha256(sha256(hex))
 *
   * @param bytes
   * @return
   */
  def doubleSHA256(bytes : List[Byte]) : DoubleSha256Digest = doubleSHA256(bytes.toSeq)

  /**
   * Performs sha256(sha256(bytes))
 *
   * @param bytes
   * @return
   */
  def doubleSHA256(bytes : Seq[Byte]) : DoubleSha256Digest = {
    val hash : List[Byte] = Sha256Hash.hashTwice(bytes.toArray).toList
    DoubleSha256Digest(hash)
  }

  /**
   * Takes sha256(hex)
 *
   * @param hex
   * @return
   */
  def sha256(hex : String) : Sha256Digest = sha256(BitcoinSUtil.decodeHex(hex))

  /**
   * Takes sha256(bytes)
 *
   * @param bytes
   * @return
   */
  def sha256(bytes : List[Byte]) : Sha256Digest = {
    val hash : List[Byte] = Sha256Hash.hash(bytes.toArray).toList
    Sha256Digest(hash)
  }

  /**
   * Performs SHA1(bytes)
 *
   * @param bytes
   * @return
   */
  def sha1(bytes : List[Byte]) : Sha1Digest = {
    val digest = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(digest)
  }



  /**
   * Performs SHA1(hex)
 *
   * @param hex
   * @return
   */
  def sha1(hex : String) : Sha1Digest = sha1(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs RIPEMD160(hex)
 *
   * @param hex
   * @return
   */
  def ripeMd160(hex : String) : RipeMd160Digest = ripeMd160(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs RIPEMD160(bytes)
 *
   * @param bytes
   * @return
   */
  def ripeMd160(bytes : List[Byte]) : RipeMd160Digest = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize())(0)
    messageDigest.doFinal(out, 0)
    RipeMd160Digest(out.toList)
  }


}

object CryptoUtil extends CryptoUtil
