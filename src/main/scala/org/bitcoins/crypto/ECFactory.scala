package org.bitcoins.crypto

import java.math.BigInteger
import java.security.SecureRandom

import org.bitcoins.config.NetworkParameters
import org.bitcoins.util.{Base58, BitcoinSUtil, Factory}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params.{ECKeyGenerationParameters, ECPrivateKeyParameters}

/**
 * Created by chris on 2/16/16.
 */
trait ECFactory extends Factory[BaseECKey] {

  /**
   * Creates a private key from a hex string
   * @param hex
   * @return
   */
  def privateKey(hex : String) : ECPrivateKey = ECPrivateKeyImpl(hex)

  /**
   * Creates a private key from a sequence of bytes
   * @param bytes
   * @return
   */
  def privateKey(bytes : Seq[Byte]) : ECPrivateKey = privateKey(BitcoinSUtil.encodeHex(bytes))

  /**
   * Generates a fresh ECPrivateKey
   * @return
   */
  def privateKey : ECPrivateKey = {
    val secureRandom = new SecureRandom
    val generator : ECKeyPairGenerator = new ECKeyPairGenerator
    val keyGenParams : ECKeyGenerationParameters = new ECKeyGenerationParameters(CryptoParams.curve, secureRandom)
    generator.init(keyGenParams)
    val keypair : AsymmetricCipherKeyPair = generator.generateKeyPair
    val privParams: ECPrivateKeyParameters = keypair.getPrivate.asInstanceOf[ECPrivateKeyParameters]
    val priv : BigInteger = privParams.getD
    privateKey(priv.toByteArray)
  }

  /**
   * Creates a public key from a hex string
   * @param hex
   * @return
   */
  def publicKey(hex : String) : ECPublicKey =  {
    if (hex == "") ECPublicKeyImpl("00")
    else ECPublicKeyImpl(hex)
  }

  /**
   * Creates a public key from a sequence of bytes
   * @param bytes
   * @return
   */
  def publicKey(bytes : Seq[Byte]) : ECPublicKey = publicKey(BitcoinSUtil.encodeHex(bytes))

  /**
   * Generates a fresh public key
   * @return
   */
  def publicKey = privateKey.publicKey


  /**
   * Creates a digital signature from the given hex string
   * @param hex
   * @return
   */
  def digitalSignature(hex : String) : ECDigitalSignature = digitalSignature(BitcoinSUtil.decodeHex(hex))

  /**
   * Creates a digital signature from the given sequence of bytes
   * @param bytes
   * @return
   */
  def digitalSignature(bytes : Seq[Byte]) : ECDigitalSignature = {
    //this represents the empty signature
    if (bytes.size == 1 && bytes.head == 0x0) EmptyDigitalSignature
    else if (bytes.size == 0) EmptyDigitalSignature
    else ECDigitalSignatureImpl(bytes)
  }

  /**
    * Takes in the r and s component of a digital signature and gives back a ECDigitalSignature object
    * The ECDigitalSignature object complies with strict der encoding as per BIP62
    * note: That the hash type for the signature CANNOT be added to the digital signature
    * @param r the r component of the digital signature
    * @param s the s component of the digital signature
    * @return
    */
  def digitalSignature(r : BigInt, s : BigInt) : ECDigitalSignature = {
    val rsSize = r.toByteArray.size + s.toByteArray.size
    val totalSize = 4 + rsSize
    val bytes : Seq[Byte] = Seq(0x30.toByte, totalSize.toByte, 0x2.toByte, r.toByteArray.size.toByte) ++
      r.toByteArray.toSeq ++ Seq(0x2.toByte, s.toByteArray.size.toByte) ++ s.toByteArray.toSeq
    digitalSignature(bytes)
  }


  /**
   * Creates a private key from a hex string
   * @param hex
   * @return
   */
  override def fromHex(hex : String) : BaseECKey = privateKey(hex)

  /**
   * Creates a private key from a byte array
   * @param bytes
   * @return
   */
  override def fromBytes(bytes : Seq[Byte]) : BaseECKey = privateKey(bytes)


  /**
   * Takes in a base58 string and converts it into a private key
   * @param base58
   * @return
   */
  def fromBase58ToPrivateKey(base58 : String) : ECPrivateKey = {
    val decodedBase58 = Base58.decode(base58)
    ECFactory.privateKey(decodedBase58)
  }

}

object ECFactory extends ECFactory
