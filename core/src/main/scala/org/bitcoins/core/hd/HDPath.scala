package org.bitcoins.core.hd

import org.bitcoins.crypto.StringFactory

import scala.util.{Failure, Success, Try}

sealed trait HDPath extends BIP32Path {

  /** This type is to give a cleaner return
    * type of `next`.
    *
    * Consider:
    *
    * {{{
    * def next: this.type = ???
    *
    * val first: SegWitHDPath = ???
    * val second = first.next
    * // second is now:
    * // first.type (with underlying type org.bitcoins.core.hd.SegWitHDPath)
    * }}}
    *
    * {{{
    * def next: NextPath = ???
    *
    * // in SegWitHDPath
    * override type NextPath = SegWitHDPath
    *
    * val first: SegWitHDPath = ???
    * val second = first.next
    * // second is now:
    * // SegWitHDPath
    * }}}
    */
  protected type NextPath <: HDPath

  /** Increments the address index and returns the
    * new path that can be passed into a
    * [[org.bitcoins.core.crypto.ExtKey ExtKey]]
    */
  def next: NextPath =
    HDAddress(chain, address.index + 1).toPath.asInstanceOf[NextPath]

  def account: HDAccount = address.account

  def purpose: HDPurpose = coin.purpose

  def coin: HDCoin = address.coin

  def chain: HDChain = address.chain

  def address: HDAddress

  override val path: Vector[BIP32Node] = address.path
}

object HDPath extends StringFactory[HDPath] {

  /** Attempts to parse a string into a valid HD path */
  override def fromStringT(string: String): Try[HDPath] =
    LegacyHDPath
      .fromStringT(string)
      .orElse(SegWitHDPath.fromStringT(string))
      .orElse(NestedSegWitHDPath.fromStringT(string))

  override def fromString(string: String): HDPath = {
    fromStringT(string) match {
      case Success(path) => path
      case Failure(exn)  => throw exn
    }
  }
}

sealed abstract class LegacyHDPath extends HDPath {
  override protected type NextPath = LegacyHDPath
}

object LegacyHDPath extends HDPathFactory[LegacyHDPath] {

  /** The purpose constant from BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#purpose BIP44]]
    */
  override val PURPOSE: Int = 44

  private case class LegacyHDPathImpl(address: HDAddress) extends LegacyHDPath

  override def apply(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): LegacyHDPath = {

    val address =
      assembleAddress(coinType, accountIndex, chainType, addressIndex)
    LegacyHDPathImpl(address)
  }

}

sealed abstract class MultisigHDPath extends HDPath {
  override protected type NextPath = MultisigHDPath
}

object MultisigHDPath extends HDPathFactory[MultisigHDPath] {

  /** The purpose constant from BIP45
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0045.mediawiki BIP45]]
    */
  override val PURPOSE: Int = 45

  private case class MultisigHDPathImpl(address: HDAddress)
      extends MultisigHDPath

  override def apply(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): MultisigHDPath = {

    val address =
      assembleAddress(coinType, accountIndex, chainType, addressIndex)
    MultisigHDPathImpl(address)
  }
}

sealed abstract class NestedSegWitHDPath extends HDPath {
  override protected type NextPath = NestedSegWitHDPath
}

object NestedSegWitHDPath extends HDPathFactory[NestedSegWitHDPath] {

  /** The purpose constant from BIP49
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
    */
  override val PURPOSE: Int = 49

  private case class NestedSegWitHDPathImpl(address: HDAddress)
      extends NestedSegWitHDPath

  override def apply(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): NestedSegWitHDPath = {

    val address =
      assembleAddress(coinType, accountIndex, chainType, addressIndex)
    NestedSegWitHDPathImpl(address)
  }
}

sealed abstract class SegWitHDPath extends HDPath {
  override protected type NextPath = SegWitHDPath
}

object SegWitHDPath extends HDPathFactory[SegWitHDPath] {

  /** The purpose constant from BIP84
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
    */
  override val PURPOSE = 84

  private case class SegWitHDPathImpl(address: HDAddress) extends SegWitHDPath

  override def apply(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): SegWitHDPath = {
    val address =
      assembleAddress(coinType, accountIndex, chainType, addressIndex)
    SegWitHDPathImpl(address)

  }
}
