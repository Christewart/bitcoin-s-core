package org.bitcoins.corecross

import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.core.config.MainNet
import scodec.bits.ByteVector

object CoreMain {

  def main(args: Array[String]): Unit = {
    val runtime = new org.bitcoins.crypto.JsCryptoRuntime
    val bigInteger = runtime.generatePrivateKey
    val privKey = ECPrivateKey.fromBigInteger(bigInteger)
    val pubKey = privKey.publicKey
    val address = P2PKHAddress(pubKey, MainNet)
    println(s"privKey=${privKey.toStringSensitive}")
    println(s"pubKey=${pubKey}")
    println(s"hash160=${runtime.sha256Hash160(ByteVector.empty)}")
    println(s"address=${address.toString}")
  }
}
