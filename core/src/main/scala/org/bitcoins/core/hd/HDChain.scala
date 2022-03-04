package org.bitcoins.core.hd

/** Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * change chain
  */
sealed abstract class HDChain extends BIP32Path {

  override val path: Vector[BIP32Node] = {
    /*    val normalizedIdx =
      if (toInt >= ExtKey.hardenedIdx.toInt) toInt - ExtKey.hardenedIdx.toInt
      else toInt*/
    val path = account.path :+ BIP32Node(toInt, hardened = false)
    println(s"path.toInt=$toInt path=${BIP32Path(path)} vec=${path}")
    path
  }

  def purpose: HDPurpose = account.purpose

  def coin: HDCoin = account.coin

  def account: HDAccount

  def chainType: HDChainType

  def toInt: Int = chainType.index

  /** Given a index, creates a HD address */
  def toHDAddress(index: Int): HDAddress = HDAddress(this, index = index)
}

object HDChain {

  private case class BIP44ChainImpl(chainType: HDChainType, account: HDAccount)
      extends HDChain

  def apply(chainType: HDChainType, account: HDAccount): HDChain =
    BIP44ChainImpl(chainType, account)

}
