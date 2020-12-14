package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.scalajs.js
import scala.scalajs.js.annotation._

import scala.scalajs.js._
import scala.scalajs.js.typedarray._
import java.math.BigInteger
import java.nio.ByteBuffer
import scala.collection.mutable

@JSExportTopLevel("JsCryptoRuntime")
class JsCryptoRuntime extends CryptoRuntime {

  override def generatePrivateKey: BigInteger = {
    //THIS IS NOT SECURE, DOING THIS FOR NOW TO
    //GET SOMETHING WORKING
    val bytes = scala.util.Random.nextBytes(32)
    new BigInteger(1, bytes)
  }

  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    val ripeMd160 = new RipeMd160
    ripeMd160.init()
    ripeMd160.update(toNodeBuffer(bytes))
    val hashBytes = ripeMd160.`final`()
    val byteVector = toByteVector(buffer = hashBytes, len = 20)
    val hash = RipeMd160Digest.fromBytes(byteVector)
    hash
  }

  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val h = new Hash160()
    h.init()
    h.update(toNodeBuffer(bytes))
    val hashBytes = h.`final`()
    val byteVector = toByteVector(buffer = hashBytes, len = 20)
    val hash =
      Sha256Hash160Digest.fromBytes(byteVector)
    hash
  }

  override def toPublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val sha256 = new SHA256
    val ecdsa = new ECDSA("SECP256K1", sha256, sha256, null)
    val privKeyBuffer = toNodeBuffer(privateKey.bytes)
    val pubKeyBuffer =
      ecdsa.publicKeyCreate(key = privKeyBuffer, compressed = true)
    val byteVector = toByteVector(pubKeyBuffer, 33)
    ECPublicKey.fromBytes(byteVector)
  }

  override def sha256(bytes: ByteVector): Sha256Digest = {
    val sha256 = new SHA256
    sha256.init()
    sha256.update(toNodeBuffer(bytes))
    val hashBytes = sha256.`final`()
    val byteVector = toByteVector(buffer = hashBytes, len = 32)
    val hash = Sha256Digest.fromBytes(byteVector)
    hash
  }

  private def toNodeBuffer(byteVector: ByteVector): Buffer = {
    //the implicit used here is this
    //https://github.com/scala-js/scala-js/blob/b5a93bb99a0b0b5044141d4b2871ea260ef17798/library/src/main/scala/scala/scalajs/js/typedarray/package.scala#L33
    Buffer.from(byteVector.toArray.toTypedArray.buffer)
  }

  private def toByteVector(buffer: Buffer, len: Int): ByteVector = {
    //is this right?
    val iter: js.Iterator[Int] = buffer.values()

    val accum = new scala.collection.mutable.ArrayBuffer[Int](len)

    var done = false
    while (!done) {
      val entry = iter.next()
      if (entry.done) {
        done = true
      } else {
        accum += entry.value
      }
    }
    require(accum.length == len,
            s"Need $len bytes for buffer -> bytevector conversion")
    ByteVector(accum.map(_.toByte))
  }
}
