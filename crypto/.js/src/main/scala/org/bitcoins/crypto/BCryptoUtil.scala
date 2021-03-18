package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.scalajs.js
import scala.scalajs.js.typedarray._

trait BCryptoUtil {

  def bufferToByteVector(b: Buffer): ByteVector = toByteVector(b, b.length)

  def byteVectorToBuffer(b: ByteVector): Buffer = toNodeBuffer(b)

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

object BCryptoUtil extends BCryptoUtil
