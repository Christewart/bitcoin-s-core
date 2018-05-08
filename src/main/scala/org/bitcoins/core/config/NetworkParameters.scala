package org.bitcoins.core.config

import org.bitcoins.core.config
import org.bitcoins.core.protocol.blockchain._

/**
 * Created by chris on 7/27/15.
 */
sealed abstract class NetworkParameters {
  /** The parameters of the blockchain we are connecting to */
  def chainParams: ChainParams

  def p2pkhNetworkByte: Seq[Byte] = chainParams.base58Prefix(Base58Type.PubKeyAddress)
  def p2shNetworkByte: Seq[Byte] = chainParams.base58Prefix(Base58Type.ScriptAddress)
  def privateKey: Seq[Byte] = chainParams.base58Prefix(Base58Type.SecretKey)

  def port: Int
  def rpcPort: Int
  def name: String = chainParams.networkId
  /** The seeds used to bootstrap the network */
  def dnsSeeds: Seq[String]

  /**
   * The message start string is designed to be unlikely to occur in normal data.
   * The characters are rarely used upper ASCII, not valid as UTF-8, and produce
   * a large 32-bit integer with any alignment.
   * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L108
   */
  def magicBytes: Seq[Byte]

  /** In bitcoin, the network recaculates the difficulty for the network every 2016 blocks */
  def difficultyChangeThreshold: Int
}

sealed abstract class BitcoinNetwork extends NetworkParameters {
  override def difficultyChangeThreshold: Int = 2016

  override def chainParams: BitcoinChainParams
}

sealed abstract class MainNet extends BitcoinNetwork {
  override def chainParams = MainNetChainParams
  override def port = 8333
  override def rpcPort = 8332
  //mainnet doesn't need to be specified like testnet or regtest
  override def name = ""
  override def dnsSeeds = Seq("seed.bitcoin.sipa.be", "dnsseed.bluematt.me", "dnsseed.bitcoin.dashjr.org",
    "seed.bitcoinstats.com", "bitseed.xf2.org", "seed.bitcoin.jonasschnelli.ch")

  override def magicBytes = Seq(0xf9.toByte, 0xbe.toByte, 0xb4.toByte, 0xd9.toByte)

  override def difficultyChangeThreshold: Int = 2016
}

object MainNet extends MainNet

sealed abstract class TestNet3 extends BitcoinNetwork {
  override def chainParams = TestNetChainParams
  override def port = 18333
  override def rpcPort = 18332
  override def dnsSeeds = Seq(
    "testnet-seed.bitcoin.petertodd.org",
    "testnet-seed.bluematt.me", "testnet-seed.bitcoin.schildbach.de")
  override def magicBytes = Seq(0x0b.toByte, 0x11.toByte, 0x09.toByte, 0x07.toByte)

  override def difficultyChangeThreshold: Int = 2016
}

object TestNet3 extends TestNet3

sealed abstract class RegTest extends BitcoinNetwork {
  override def chainParams = RegTestNetChainParams
  override def port = 18444
  override def rpcPort = TestNet3.rpcPort
  override def dnsSeeds = Nil
  override def magicBytes = Seq(0xfa.toByte, 0xbf.toByte, 0xb5.toByte, 0xda.toByte)
  override def difficultyChangeThreshold: Int = 2016
}

object RegTest extends RegTest

sealed abstract class ZCashNetwork extends NetworkParameters

/**
 * ZCash's main network parameters
 * [[https://github.com/zcash/zcash/blob/138cf7700457b08ad7993c40d26da2f425387daf/src/chainparams.cpp#L81]]
 */
sealed abstract class ZCashMainNet extends ZCashNetwork {
  override def chainParams: ChainParams = ZCashMainNetChainParams
  override def rpcPort: Int = 8232
  override def port = 8233
  override def difficultyChangeThreshold: Int = ???

  override def dnsSeeds: Seq[String] = Seq(
    "dnsseed.z.cash",
    "dnsseed.str4d.xyz",
    "dnsseed.znodes.org")

  override def magicBytes: Seq[Byte] = Seq(0x24.toByte, 0xe9.toByte, 0x27.toByte, 0x64.toByte)
}

object ZCashMainNet extends ZCashMainNet

/**
 * ZCash's test network parameters
 * [[https://github.com/zcash/zcash/blob/138cf7700457b08ad7993c40d26da2f425387daf/src/chainparams.cpp#L242]]
 */
sealed abstract class ZCashTestNet extends ZCashNetwork {
  override def chainParams: ChainParams = ZCashTestNetChainParams
  override def rpcPort: Int = 18232
  override def port: Int = 8233

  override def dnsSeeds: Seq[String] = Seq(
    "dnsseed.testnet.z.cash")
  override def difficultyChangeThreshold: Int = ???

  override def magicBytes: Seq[Byte] = Seq(0xfa.toByte, 0x1a.toByte, 0xf9.toByte, 0xbf.toByte)

}
object ZCashTestNet extends ZCashTestNet

/**
 * ZCash's regtest network parameters
 * [[https://github.com/zcash/zcash/blob/138cf7700457b08ad7993c40d26da2f425387daf/src/chainparams.cpp#L351]]
 */
sealed abstract class ZCashRegTest extends ZCashNetwork {
  override def chainParams: ChainParams = ZCashRegTestChainParams
  override def rpcPort = ZCashTestNet.rpcPort
  override def port = ZCashTestNet.port

  override def dnsSeeds: Seq[String] = Nil
  override def difficultyChangeThreshold: Int = ???

  override def magicBytes: Seq[Byte] = Seq(0xaa.toByte, 0xe8.toByte, 0x3f.toByte, 0x5f.toByte)
}

object ZCashRegTest extends ZCashRegTest

object BitcoinNetworks extends Networks {
  override val knownNetworks = Seq(MainNet, TestNet3, RegTest)

  override def bytesToNetwork: Map[Seq[Byte], NetworkParameters] = Map(
    MainNet.p2pkhNetworkByte -> MainNet,
    MainNet.p2shNetworkByte -> MainNet,
    MainNet.privateKey -> MainNet,

    TestNet3.p2pkhNetworkByte -> TestNet3,
    TestNet3.p2shNetworkByte -> TestNet3,
    TestNet3.privateKey -> TestNet3
  //omitting regtest as it has the network same network bytes as regtest
  )
}

object ZCashNetworks extends Networks {
  val knownNetworks: Seq[ZCashNetwork] = Seq(ZCashMainNet, ZCashTestNet, ZCashRegTest)

  override def bytesToNetwork: Map[Seq[Byte], NetworkParameters] = Map(
    ZCashMainNet.p2pkhNetworkByte -> ZCashMainNet,
    ZCashMainNet.p2shNetworkByte -> ZCashMainNet,
    ZCashMainNet.privateKey -> ZCashMainNet,

    ZCashTestNet.p2pkhNetworkByte -> ZCashTestNet,
    ZCashTestNet.p2shNetworkByte -> ZCashTestNet,
    ZCashTestNet.privateKey -> ZCashTestNet
  //ommitting regtest as it has the same network bytes as ZCashTestNet
  )

}

sealed abstract class Networks {
  val knownNetworks: Seq[NetworkParameters]

  def bytesToNetwork: Map[Seq[Byte], NetworkParameters]
}

object Networks extends Networks {
  val bitcoinNetworks = BitcoinNetworks.knownNetworks
  val zcashNetworks = ZCashNetworks.knownNetworks
  val knownNetworks: Seq[NetworkParameters] = bitcoinNetworks ++ zcashNetworks

  val secretKeyBytes = knownNetworks.map(_.privateKey)
  val p2pkhNetworkBytes = knownNetworks.map(_.p2pkhNetworkByte)
  val p2shNetworkBytes = knownNetworks.map(_.p2shNetworkByte)

  def bytesToNetwork: Map[Seq[Byte], NetworkParameters] = BitcoinNetworks.bytesToNetwork ++ ZCashNetworks.bytesToNetwork
}