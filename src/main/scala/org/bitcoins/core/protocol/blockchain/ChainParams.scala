package org.bitcoins.core.protocol.blockchain

import java.nio.charset.StandardCharsets

import org.bitcoins.core.consensus.Merkle
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{ CurrencyUnit, CurrencyUnits, Satoshis }
import org.bitcoins.core.number.{ Int32, Int64, UInt32 }
import org.bitcoins.core.protocol.script.{ ScriptPubKey, ScriptSignature }
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.{ BytesToPushOntoStack, ScriptConstant, ScriptNumber }
import org.bitcoins.core.script.crypto.OP_CHECKSIG
import org.bitcoins.core.util.{ BitcoinSUtil, BitcoinScriptUtil }

/**
 * Created by chris on 5/22/16.
 * CChainParams defines various tweakable parameters of a given instance of the
 * Bitcoin system. There are three: the main network on which people trade goods
 * and services, the public test network which gets reset from time to time and
 * a regression test mode which is intended for private networks only. It has
 * minimal difficulty to ensure that blocks can be found instantly.
 * Mimics this C++ interface
 * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.h#L42
 */
sealed abstract class ChainParams {

  /** Return the BIP70 network string ([[MainNetChainParams]], [[TestNetChainParams]] or [[RegTestNetChainParams]].) */
  def networkId: String

  /** The Genesis [[Block]] in the blockchain. */
  def genesisBlock: Block

  /**
   * Filter transactions that do not match well-defined patterns
   * inside of [[org.bitcoins.core.policy.Policy]].
   */
  def requireStandardTransaction: Boolean = true

  /** Takes in a [[Base58Type]] and returns its base58 prefix. */
  def base58Prefix(base58: Base58Type): Seq[Byte] = base58Prefixes(base58)

  /**
   * The mapping from a [[Base58Type]]to a String.
   * Base58 prefixes for various keys/hashes on the network.
   * See: [[https://en.bitcoin.it/wiki/List_of_address_prefixes]].
   */
  def base58Prefixes: Map[Base58Type, Seq[Byte]]

  /**
   * Creates the Genesis [[Block]] for this blockchain.
   * Mimics this function in bitcoin core:
   * [[https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L51]]
   * @param time the time when the miner started hashing the block header
   * @param nonce the nonce to mine the block
   * @param nBits An encoded version of the target threshold this block’s header hash must be less than or equal to.
   * @param version the block version
   * @param amount the block reward for the genesis block (50 BTC in Bitcoin)
   * @return the newly minted genesis block
   */
  def createGenesisBlock(time: UInt32, nonce: UInt32, nBits: UInt32, version: UInt32, amount: CurrencyUnit): Block = {
    val timestamp = "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    val asm = Seq(BytesToPushOntoStack(65), ScriptConstant("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"), OP_CHECKSIG)
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    createGenesisBlock(timestamp, genesisOutputScript, time, nonce, nBits, version, amount)
  }

  /**
   * @param timestamp a piece of data to signify when this block was first created - satoshi used an article headline
   * @param scriptPubKey the scriptPubKey that needs to be satisfied in able to spend the genesis block reward
   * @param time the time when the miner started hashing the block header
   * @param nonce the nonce used to mine the block
   * @param nBits An encoded version of the target threshold this block's header hash must be less than or equal to
   * @param version the block version
   * @param amount the block reward for the genesis block (50 BTC in Bitcoin)
   * @return the newly minted genesis block
   */
  def createGenesisBlock(timestamp: String, scriptPubKey: ScriptPubKey, time: UInt32, nonce: UInt32, nBits: UInt32,
    version: UInt32, amount: CurrencyUnit): Block = {
    val timestampBytes = timestamp.getBytes(StandardCharsets.UTF_8)
    //see https://bitcoin.stackexchange.com/questions/13122/scriptsig-coinbase-structure-of-the-genesis-block
    //for a full breakdown of the genesis block & its script signature
    val const = ScriptConstant(timestampBytes)
    val scriptSignature = ScriptSignature.fromAsm(Seq(BytesToPushOntoStack(4), ScriptNumber(486604799),
      BytesToPushOntoStack(1), ScriptNumber(4)) ++ BitcoinScriptUtil.calculatePushOp(const) ++ Seq(const))
    val input = CoinbaseInput(scriptSignature)
    val output = TransactionOutput(amount, scriptPubKey)
    val tx = BaseTransaction(TransactionConstants.version, Seq(input), Seq(output), TransactionConstants.lockTime)
    val prevBlockHash = DoubleSha256Digest("0000000000000000000000000000000000000000000000000000000000000000")
    val merkleRootHash = Merkle.computeMerkleRoot(Seq(tx))
    val genesisBlockHeader = BitcoinBlockHeader(version, prevBlockHash, merkleRootHash, time, nBits, nonce)
    val genesisBlock = Block(genesisBlockHeader, Seq(tx))
    genesisBlock
  }
}

sealed abstract class BitcoinChainParams extends ChainParams
/** The Main Network parameters. */
object MainNetChainParams extends BitcoinChainParams {

  override def networkId = "main"

  override def genesisBlock: Block = createGenesisBlock(UInt32(1231006505), UInt32(2083236893), UInt32(0x1d00ffff), UInt32.one, Satoshis(Int64(5000000000L)))

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    Base58Type.PubKeyAddress -> BitcoinSUtil.decodeHex("00"),
    Base58Type.ScriptAddress -> BitcoinSUtil.decodeHex("05"),
    Base58Type.SecretKey -> BitcoinSUtil.decodeHex("80"),
    Base58Type.ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("b2"), BitcoinSUtil.hexToByte("1e")),
    Base58Type.ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("ad"), BitcoinSUtil.hexToByte("e4")))
}

object TestNetChainParams extends BitcoinChainParams {

  override def networkId = "test"

  override def genesisBlock: Block = createGenesisBlock(UInt32(1296688602), UInt32(414098458), UInt32(0x1d00ffff), UInt32.one, Satoshis(Int64(5000000000L)))

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    Base58Type.PubKeyAddress -> BitcoinSUtil.decodeHex("6f"),
    Base58Type.ScriptAddress -> BitcoinSUtil.decodeHex("c4"),
    Base58Type.SecretKey -> BitcoinSUtil.decodeHex("ef"),
    Base58Type.ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("87"), BitcoinSUtil.hexToByte("cf")),
    Base58Type.ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("83"), BitcoinSUtil.hexToByte("94")))
}

object RegTestNetChainParams extends BitcoinChainParams {
  override def networkId = "regtest"
  override def genesisBlock: Block = createGenesisBlock(UInt32(1296688602), UInt32(2), UInt32(0x207fffff), UInt32.one, Satoshis(Int64(5000000000L)))
  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = TestNetChainParams.base58Prefixes
}

/** A abstract class to represent the ZCash cryptocurrency's chain parameters */
sealed abstract class ZCashChainParams extends ChainParams {
  def createGenesisBlockZCash(timestamp: String, genesisOutputScript: ScriptPubKey, time: UInt32, nonce: UInt32,
    nSolution: Seq[Byte], nBits: UInt32, nVersion: Int32, genesisReward: CurrencyUnit): Block = {

    ???
  }

  def createGenesisBlockZCash(time: UInt32, nonce: UInt32, nSolution: Seq[Byte], nBits: UInt32,
    nVersion: Int32, genesisReward: CurrencyUnit): Block = {
    val timestamp = "Zcash0b9c4eef8b7cc417ee5001e3500984b6fea35683a7cac141a043c42064835d34"
    val constant = ScriptConstant("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f")
    val asm = BitcoinScriptUtil.calculatePushOp(constant) ++ Seq(constant, OP_CHECKSIG)
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    createGenesisBlockZCash(timestamp, genesisOutputScript, time, nonce, nSolution, nBits, nVersion, genesisReward)
  }
}
/**
 * ZCash MainNetwork chain parameters
 * [[https://github.com/zcash/zcash/blob/138cf7700457b08ad7993c40d26da2f425387daf/src/chainparams.cpp#L81]]
 */
object ZCashMainNetChainParams extends ZCashChainParams {
  override def networkId = "main"
  /** Creates the ZCash main network genesis block */
  override def genesisBlock: Block = {
    val hex = "000a889f00854b8665cd555f4656f68179d31ccadc1b1f7fb0952726313b16941da348284d67add4686121d4e3d930160c1348d8191c25f12b267a6a9c131b5031cbf8af1f79c9d513076a216ec87ed045fa966e01214ed83ca02dc1797270a454720d3206ac7d931a0a680c5c5e099057592570ca9bdf6058343958b31901fce1a15a4f38fd347750912e14004c73dfe588b903b6c03166582eeaf30529b14072a7b3079e3a684601b9b3024054201f7440b0ee9eb1a7120ff43f713735494aa27b1f8bab60d7f398bca14f6abb2adbf29b04099121438a7974b078a11635b594e9170f1086140b4173822dd697894483e1c6b4e8b8dcd5cb12ca4903bc61e108871d4d915a9093c18ac9b02b6716ce1013ca2c1174e319c1a570215bc9ab5f7564765f7be20524dc3fdf8aa356fd94d445e05ab165ad8bb4a0db096c097618c81098f91443c719416d39837af6de85015dca0de89462b1d8386758b2cf8a99e00953b308032ae44c35e05eb71842922eb69797f68813b59caf266cb6c213569ae3280505421a7e3a0a37fdf8e2ea354fc5422816655394a9454bac542a9298f176e211020d63dee6852c40de02267e2fc9d5e1ff2ad9309506f02a1a71a0501b16d0d36f70cdfd8de78116c0c506ee0b8ddfdeb561acadf31746b5a9dd32c21930884397fb1682164cb565cc14e089d66635a32618f7eb05fe05082b8a3fae620571660a6b89886eac53dec109d7cbb6930ca698a168f301a950be152da1be2b9e07516995e20baceebecb5579d7cdbc16d09f3a50cb3c7dffe33f26686d4ff3f8946ee6475e98cf7b3cf9062b6966e838f865ff3de5fb064a37a21da7bb8dfd2501a29e184f207caaba364f36f2329a77515dcb710e29ffbf73e2bbd773fab1f9a6b005567affff605c132e4e4dd69f36bd201005458cfbd2c658701eb2a700251cefd886b1e674ae816d3f719bac64be649c172ba27a4fd55947d95d53ba4cbc73de97b8af5ed4840b659370c556e7376457f51e5ebb66018849923db82c1c9a819f173cccdb8f3324b239609a300018d0fb094adf5bd7cbb3834c69e6d0b3798065c525b20f040e965e1a161af78ff7561cd874f5f1b75aa0bc77f720589e1b810f831eac5073e6dd46d00a2793f70f7427f0f798f2f53a67e615e65d356e66fe40609a958a05edb4c175bcc383ea0530e67ddbe479a898943c6e3074c6fcc252d6014de3a3d292b03f0d88d312fe221be7be7e3c59d07fa0f2f4029e364f1f355c5d01fa53770d0cd76d82bf7e60f6903bc1beb772e6fde4a70be51d9c7e03c8d6d8dfb361a234ba47c470fe630820bbd920715621b9fbedb49fcee165ead0875e6c2b1af16f50b5d6140cc981122fcbcf7c5a4e3772b3661b628e08380abc545957e59f634705b1bbde2f0b4e055a5ec5676d859be77e20962b645e051a880fddb0180b4555789e1f9344a436a84dc5579e2553f1e5fb0a599c137be36cabbed0319831fea3fddf94ddc7971e4bcf02cdc93294a9aab3e3b13e3b058235b4f4ec06ba4ceaa49d675b4ba80716f3bc6976b1fbf9c8bf1f3e3a4dc1cd83ef9cf816667fb94f1e923ff63fef072e6a19321e4812f96cb0ffa864da50ad74deb76917a336f31dce03ed5f0303aad5e6a83634f9fcc371096f8288b8f02ddded5ff1bb9d49331e4a84dbe1543164438fde9ad71dab024779dcdde0b6602b5ae0a6265c14b94edd83b37403f4b78fcd2ed555b596402c28ee81d87a909c4e8722b30c71ecdd861b05f61f8b1231795c76adba2fdefa451b283a5d527955b9f3de1b9828e7b2e74123dd47062ddcc09b05e7fa13cb2212a6fdbc65d7e852cec463ec6fd929f5b8483cf3052113b13dac91b69f49d1b7d1aec01c4a68e41ce157"
    val block = createGenesisBlock(UInt32(1477641360), UInt32(1257), UInt32("1f07ffff"), UInt32(4), CurrencyUnits.zero)
    val hash = block.blockHeader.hash
    val merkleRootHash = block.blockHeader.merkleRootHash
    val fe = BitcoinSUtil.flipEndianness(_: String)
    require(merkleRootHash == DoubleSha256Digest(fe("c4eaa58879081de3c24a7b117ed2b28300e7ec4c4c1dff1d3f1268b7857a4ddb")))
    require(DoubleSha256Digest(fe("00040fe8ec8471911baa1db1266ea15dd06b4a8a5c453883c000b031973dce08")) == hash)
    block
  }

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    Base58Type.PubKeyAddress -> BitcoinSUtil.decodeHex("1cb8"),
    Base58Type.ScriptAddress -> BitcoinSUtil.decodeHex("1cbd"),
    Base58Type.SecretKey -> BitcoinSUtil.decodeHex("80"),
    Base58Type.ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("b2"), BitcoinSUtil.hexToByte("1e")),
    Base58Type.ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("ad"), BitcoinSUtil.hexToByte("e4")))

}

/**
 * ZCash Testnet Chain parameters
 * [[https://github.com/zcash/zcash/blob/138cf7700457b08ad7993c40d26da2f425387daf/src/chainparams.cpp#L242]]
 */
object ZCashTestNetChainParams extends ZCashChainParams {
  override def networkId = "test"
  override def genesisBlock: Block = {
    val hex = "000a889f00854b8665cd555f4656f68179d31ccadc1b1f7fb0952726313b16941da348284d67add4686121d4e3d930160c1348d8191c25f12b267a6a9c131b5031cbf8af1f79c9d513076a216ec87ed045fa966e01214ed83ca02dc1797270a454720d3206ac7d931a0a680c5c5e099057592570ca9bdf6058343958b31901fce1a15a4f38fd347750912e14004c73dfe588b903b6c03166582eeaf30529b14072a7b3079e3a684601b9b3024054201f7440b0ee9eb1a7120ff43f713735494aa27b1f8bab60d7f398bca14f6abb2adbf29b04099121438a7974b078a11635b594e9170f1086140b4173822dd697894483e1c6b4e8b8dcd5cb12ca4903bc61e108871d4d915a9093c18ac9b02b6716ce1013ca2c1174e319c1a570215bc9ab5f7564765f7be20524dc3fdf8aa356fd94d445e05ab165ad8bb4a0db096c097618c81098f91443c719416d39837af6de85015dca0de89462b1d8386758b2cf8a99e00953b308032ae44c35e05eb71842922eb69797f68813b59caf266cb6c213569ae3280505421a7e3a0a37fdf8e2ea354fc5422816655394a9454bac542a9298f176e211020d63dee6852c40de02267e2fc9d5e1ff2ad9309506f02a1a71a0501b16d0d36f70cdfd8de78116c0c506ee0b8ddfdeb561acadf31746b5a9dd32c21930884397fb1682164cb565cc14e089d66635a32618f7eb05fe05082b8a3fae620571660a6b89886eac53dec109d7cbb6930ca698a168f301a950be152da1be2b9e07516995e20baceebecb5579d7cdbc16d09f3a50cb3c7dffe33f26686d4ff3f8946ee6475e98cf7b3cf9062b6966e838f865ff3de5fb064a37a21da7bb8dfd2501a29e184f207caaba364f36f2329a77515dcb710e29ffbf73e2bbd773fab1f9a6b005567affff605c132e4e4dd69f36bd201005458cfbd2c658701eb2a700251cefd886b1e674ae816d3f719bac64be649c172ba27a4fd55947d95d53ba4cbc73de97b8af5ed4840b659370c556e7376457f51e5ebb66018849923db82c1c9a819f173cccdb8f3324b239609a300018d0fb094adf5bd7cbb3834c69e6d0b3798065c525b20f040e965e1a161af78ff7561cd874f5f1b75aa0bc77f720589e1b810f831eac5073e6dd46d00a2793f70f7427f0f798f2f53a67e615e65d356e66fe40609a958a05edb4c175bcc383ea0530e67ddbe479a898943c6e3074c6fcc252d6014de3a3d292b03f0d88d312fe221be7be7e3c59d07fa0f2f4029e364f1f355c5d01fa53770d0cd76d82bf7e60f6903bc1beb772e6fde4a70be51d9c7e03c8d6d8dfb361a234ba47c470fe630820bbd920715621b9fbedb49fcee165ead0875e6c2b1af16f50b5d6140cc981122fcbcf7c5a4e3772b3661b628e08380abc545957e59f634705b1bbde2f0b4e055a5ec5676d859be77e20962b645e051a880fddb0180b4555789e1f9344a436a84dc5579e2553f1e5fb0a599c137be36cabbed0319831fea3fddf94ddc7971e4bcf02cdc93294a9aab3e3b13e3b058235b4f4ec06ba4ceaa49d675b4ba80716f3bc6976b1fbf9c8bf1f3e3a4dc1cd83ef9cf816667fb94f1e923ff63fef072e6a19321e4812f96cb0ffa864da50ad74deb76917a336f31dce03ed5f0303aad5e6a83634f9fcc371096f8288b8f02ddded5ff1bb9d49331e4a84dbe1543164438fde9ad71dab024779dcdde0b6602b5ae0a6265c14b94edd83b37403f4b78fcd2ed555b596402c28ee81d87a909c4e8722b30c71ecdd861b05f61f8b1231795c76adba2fdefa451b283a5d527955b9f3de1b9828e7b2e74123dd47062ddcc09b05e7fa13cb2212a6fdbc65d7e852cec463ec6fd929f5b8483cf3052113b13dac91b69f49d1b7d1aec01c4a68e41ce157"
    val block = createGenesisBlockZCash(UInt32(1477641360), UInt32(1257), BitcoinSUtil.decodeHex(hex), UInt32("1f07ffff"), Int32(4), CurrencyUnits.zero)
    val hash = block.blockHeader.hash
    val merkleRootHash = block.blockHeader.merkleRootHash
    val fe = BitcoinSUtil.flipEndianness(_: String)
    require(merkleRootHash == DoubleSha256Digest(fe("c4eaa58879081de3c24a7b117ed2b28300e7ec4c4c1dff1d3f1268b7857a4ddb")))
    require(DoubleSha256Digest(fe("05a60a92d99d85997cce3b87616c089f6124d7342af37106edc76126334a2c38")) == hash)
    block
  }

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    Base58Type.PubKeyAddress -> BitcoinSUtil.decodeHex("1d25"),
    Base58Type.ScriptAddress -> BitcoinSUtil.decodeHex("1cba"),
    Base58Type.SecretKey -> BitcoinSUtil.decodeHex("ef"),
    Base58Type.ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("87"), BitcoinSUtil.hexToByte("cf")),
    Base58Type.ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("83"), BitcoinSUtil.hexToByte("94")))
}

/**
 * ZCash regtest network parameters
 * [[https://github.com/zcash/zcash/blob/138cf7700457b08ad7993c40d26da2f425387daf/src/chainparams.cpp#L351]]
 */
object ZCashRegTestChainParams extends ZCashChainParams {
  override def networkId = "regtest"
  override def genesisBlock: Block = {
    val hex = "01936b7db1eb4ac39f151b8704642d0a8bda13ec547d54cd5e43ba142fc6d8877cab07b3"
    createGenesisBlockZCash(UInt32(1296688602), UInt32(9), BitcoinSUtil.decodeHex(hex), UInt32("200f0f0f"), Int32(4), CurrencyUnits.zero)
  }

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = ZCashTestNetChainParams.base58Prefixes
}

sealed abstract class Base58Type
object Base58Type {
  case object PubKeyAddress extends Base58Type
  case object ScriptAddress extends Base58Type
  case object SecretKey extends Base58Type
  case object ExtPublicKey extends Base58Type
  case object ExtSecretKey extends Base58Type
}