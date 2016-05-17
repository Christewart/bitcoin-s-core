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
   * Signs a given sequence of bytes with the signingKey
   * @param bytes the bytes to be signed
   * @param signingKey the key to sign the bytes with
   * @return the digital signature
   */
  def sign(bytes : Seq[Byte], signingKey : BaseECKey) : ECDigitalSignature = {
    logger.debug("1")
    val signer: ECDSASigner = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
    logger.debug("2")
    val privKey: ECPrivateKeyParameters = new ECPrivateKeyParameters(new BigInteger(bytes.toArray), CryptoParams.curve)
    logger.debug("3")
    signer.init(true, privKey)
    logger.debug("4")
    val components : Array[BigInteger] = signer.generateSignature(bytes.toArray)
    logger.debug("5")
    val (r,s) = (components(0),components(1))
    logger.debug("6")
    ECFactory.digitalSignature(r,s)
/*    val bitcoinjKey = ECKey.fromPrivate(signingKey.bytes.toArray)
    val sha256Hash = Sha256Hash.wrap(bytes.toArray)
    val sigBytes : Array[Byte] = bitcoinjKey.sign(sha256Hash).encodeToDER()*/
    //ECFactory.digitalSignature(sigBytes.toSeq)
  }

  def sign(hex : String, signingKey : BaseECKey) : ECDigitalSignature = sign(BitcoinSUtil.decodeHex(hex),signingKey)

  def sign(hex : String) : ECDigitalSignature = sign(hex,this)

  def sign(bytes : Seq[Byte]) : ECDigitalSignature = sign(bytes,this)


}
