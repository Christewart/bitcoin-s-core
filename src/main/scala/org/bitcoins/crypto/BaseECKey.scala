package org.bitcoins.crypto

import java.math.BigInteger

import org.bitcoins.util.{BitcoinSLogger, BitcoinSUtil}
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.params.ECPrivateKeyParameters
import org.spongycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}

/**
 * Created by chris on 2/16/16.
 */
trait BaseECKey extends BitcoinSLogger {
  def hex : String

  def bytes : Seq[Byte] = BitcoinSUtil.decodeHex(hex)

  /**
    * Use compressed keys by default
    * @return
    */
  def compressed : Boolean = true

  /**
   * Signs a given sequence of bytes with the signingKey
   * @param bytes the bytes to be signed
   * @param signingKey the key to sign the bytes with
   * @return the digital signature
   */
  def sign(bytes : Seq[Byte], signingKey : BaseECKey) : ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters = new ECPrivateKeyParameters(
      new BigInteger(signingKey.bytes.toArray), CryptoParams.curve)
    signer.init(true, privKey)
    val components : Array[BigInteger] = signer.generateSignature(signingKey.bytes.toArray)
    val (s,r) = (components(0),components(1))
    ECFactory.digitalSignature(r,s)
  }

  def sign(hex : String, signingKey : BaseECKey) : ECDigitalSignature = sign(BitcoinSUtil.decodeHex(hex),signingKey)

  def sign(hex : String) : ECDigitalSignature = sign(hex,this)

  def sign(bytes : Seq[Byte]) : ECDigitalSignature = sign(bytes,this)


}
