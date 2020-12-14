package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger

/** Trait that should be extended by specific runtimes like javascript
  * or the JVM to support crypto functions needed for bitcoin-s
  */
trait CryptoRuntime {
  def generatePrivateKey: BigInteger
  def ripeMd160(bytes: ByteVector): RipeMd160Digest
  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest
  def toPublicKey(privateKey: ECPrivateKey): ECPublicKey
  def sha256(bytes: ByteVector): Sha256Digest

  /** Performs sha256(sha256(bytes)). */
  def doubleSHA256(bytes: ByteVector): DoubleSha256Digest = {
    val hash: ByteVector = sha256(sha256(bytes).bytes).bytes
    DoubleSha256Digest(hash)
  }
}
