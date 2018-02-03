package org.bitcoins.core.protocol.blockchain

import java.nio.charset.StandardCharsets

import org.bitcoins.core.consensus.Merkle
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32, UInt64, UInt8}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.BCashChainParams.h2b
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionInput, TransactionOutput}
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant, ScriptNumber}
import org.bitcoins.core.script.crypto.OP_CHECKSIG
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil}

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
  val logger = BitcoinSLogger.logger

  /** Return the BIP70 network string ([[MainNetChainParams]], [[TestNetChainParams]] or [[RegTestNetChainParams]].) */
  def networkId : String

  /** The Genesis [[Block]] in the blockchain. */
  def genesisBlock : Block

  /** Filter transactions that do not match well-defined patterns
    * inside of [[org.bitcoins.core.policy.Policy]]. */
  def requireStandardTransaction : Boolean

  /** Takes in a [[Base58Type]] and returns its base58 prefix. */
  def base58Prefix(base58 : Base58Type) : Seq[Byte] = base58Prefixes(base58)

  /** The mapping from a [[Base58Type]]to a String.
    * Base58 prefixes for various keys/hashes on the network.
    * See: [[https://en.bitcoin.it/wiki/List_of_address_prefixes]]. */
  def base58Prefixes : Map[Base58Type,Seq[Byte]]

  /** Creates the Genesis [[Block]] for this blockchain.
    * Mimics this function in bitcoin core:
    * [[https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L51]]
    * @param time the time when the miner started hashing the block header
    * @param nonce the nonce to mine the block
    * @param nBits An encoded version of the target threshold this block’s header hash must be less than or equal to.
    * @param version the block version
    * @param amount the block reward for the genesis block (50 BTC in Bitcoin)
    * @return the newly minted genesis block
    */
  def createGenesisBlock(time : UInt32, nonce : UInt32, nBits : UInt32, version : UInt32, amount : CurrencyUnit) : Block = {
    val timestamp = "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    val asm = Seq(BytesToPushOntoStack(65), ScriptConstant("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"), OP_CHECKSIG)
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    createGenesisBlock(timestamp,genesisOutputScript,time,nonce,nBits,version,amount)
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
  def createGenesisBlock(timestamp : String, scriptPubKey : ScriptPubKey, time : UInt32, nonce : UInt32, nBits : UInt32,
                         version : UInt32, amount : CurrencyUnit) : Block = {
    val timestampBytes: Seq[Byte] = timestamp.getBytes(StandardCharsets.UTF_8)
    //see https://bitcoin.stackexchange.com/questions/13122/scriptsig-coinbase-structure-of-the-genesis-block
    //for a full breakdown of the genesis block & its script signature
    val timestampConst = ScriptConstant(timestampBytes)
    val pushop = BitcoinScriptUtil.calculatePushOp(timestampConst)
    val scriptSignature = ScriptSignature.fromAsm(Seq(BytesToPushOntoStack(4), ScriptNumber(486604799),
      BytesToPushOntoStack(1), ScriptNumber(4)) ++ pushop ++ Seq(timestampConst))
    val input = TransactionInput(scriptSignature)
    val output = TransactionOutput(amount,scriptPubKey)
    val tx = Transaction(UInt32.one,Seq(input), Seq(output), TransactionConstants.lockTime)
    val prevBlockHash = DoubleSha256Digest("0000000000000000000000000000000000000000000000000000000000000000")
    val merkleRootHash = Merkle.computeMerkleRoot(Seq(tx))
    //require(merkleRootHash == tx.txId)
    val genesisBlockHeader = BlockHeader(version,prevBlockHash,merkleRootHash,time,nBits,nonce)
    val genesisBlock = Block(genesisBlockHeader,Seq(tx))
    genesisBlock
  }
}

/** The Main Network parameters. */
object MainNetChainParams extends ChainParams {

  override def networkId = "main"

  override def genesisBlock: Block = {
    val b = createGenesisBlock(UInt32(1231006505), UInt32(2083236893), UInt32(0x1d00ffff), UInt32.one, Satoshis(Int64(5000000000L)))
    val expectedRoot = BitcoinSUtil.flipEndianness("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
    val expected = BitcoinSUtil.flipEndianness("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
    require(b.blockHeader.merkleRootHash.hex == expectedRoot, "Got root: " + b.blockHeader.merkleRootHash.hex + "\nexpected: " + expected)
    require(b.blockHeader.hash.hex == expected, "Got " + b.blockHeader.hash.hex + "\nexpected " + expected)
    b
  }

  override def requireStandardTransaction : Boolean = true

  override def base58Prefixes : Map[Base58Type,Seq[Byte]] = Map(
    PubKeyAddress -> UInt8.zero.bytes,
    ScriptAddress -> UInt8(5).bytes,
    SecretKey -> UInt8(128).bytes,
    ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("b2"), BitcoinSUtil.hexToByte("1e")),
    ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("ad"), BitcoinSUtil.hexToByte("e4")))
}

object TestNetChainParams extends ChainParams {

  override def networkId = "test"

  override def genesisBlock : Block = createGenesisBlock(UInt32(1296688602), UInt32(414098458), UInt32(0x1d00ffff), UInt32.one, Satoshis(Int64(5000000000L)))

  override def requireStandardTransaction : Boolean = true

  override def base58Prefixes : Map[Base58Type,Seq[Byte]] = Map(
    PubKeyAddress -> BitcoinSUtil.decodeHex("6f"),
    ScriptAddress -> BitcoinSUtil.decodeHex("c4"),
    SecretKey -> BitcoinSUtil.decodeHex("ef"),
    ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("87"), BitcoinSUtil.hexToByte("cf")),
    ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("83"), BitcoinSUtil.hexToByte("94")))
}


object RegTestNetChainParams extends ChainParams {
  override def networkId = "regtest"
  override def genesisBlock : Block = createGenesisBlock(UInt32(1296688602), UInt32(2), UInt32(0x207fffff), UInt32.one, Satoshis(Int64(5000000000L)))
  override def requireStandardTransaction : Boolean = TestNetChainParams.requireStandardTransaction
  override def base58Prefixes : Map[Base58Type, Seq[Byte]] = TestNetChainParams.base58Prefixes
}

/**
  * https://github.com/BitcoinUnlimited/BitcoinUnlimited/blob/release/src/chainparams.cpp#L91
  */
object BCashChainParams extends ChainParams {
  override def networkId: String = MainNetChainParams.networkId
  override def genesisBlock = MainNetChainParams.genesisBlock
  private val h2b = BitcoinSUtil.hexToByte(_)
  override def base58Prefixes = Map(
    PubKeyAddress -> UInt8.zero.bytes,
    ScriptAddress -> UInt8(5).bytes,
    SecretKey -> UInt8(128).bytes,
    //(0x04)(0x88)(0xB2)(0x1E)
    ExtPublicKey -> Seq(h2b("04"), h2b("88"), h2b("b2"), h2b("1e")),
    //(0x04)(0x88)(0xAD)(0xE4)
    ExtSecretKey -> Seq(h2b("04"), h2b("88"), h2b("ad"), h2b("e4"))
  )

  override def requireStandardTransaction: Boolean = true

}

/**
  * https://github.com/litecoin-project/litecoin/blob/master/src/chainparams.cpp#L73
  */
object LtcChainParams extends ChainParams {
  private val h2b = BitcoinSUtil.hexToByte(_)
  override def networkId: String = MainNetChainParams.networkId
  override def genesisBlock: Block = {
    //Litecoin does not share a genesis block with bitcoin, so we need to define the ltc genesis block here
    //CreateGenesisBlock(1317972665, 2084524493, 0x1e0ffff0, 1, 50 * COIN);
    //CreateGenesisBlock(1317972665, 2084524493, 0x1e0ffff0, 1, 50 * COIN)
    val b = createGenesisBlock(UInt32(1317972665),UInt32(2084524493),UInt32(0x1e0ffff0),UInt32.one,Satoshis(Int64(5000000000L)))
    val expectedLtcMerkleRoot = BitcoinSUtil.flipEndianness("97ddfbbae6be97fd6cdf3e7ca13232a3afff2353e29badfab7f73011edd4ced9")
    val expectedLtcGenesis = BitcoinSUtil.flipEndianness("12a765e31ffd4059bada1e25190f6e98c99d9714d334efa41a195a7e7e04bfe2")
    require(b.blockHeader.merkleRootHash.hex == expectedLtcMerkleRoot, "Got merkleRoot: " + b.blockHeader.merkleRootHash.hex + "\nexpected " + expectedLtcMerkleRoot)
    require(b.blockHeader.hash.hex == expectedLtcGenesis, "Got " + b.blockHeader.hash.hex + "\nexpected " + expectedLtcGenesis)
    b
  }

  override def createGenesisBlock(time: UInt32, nonce: UInt32, nBits: UInt32, version: UInt32, amount: CurrencyUnit): Block = {
    //const char* pszTimestamp = "NY Times 05/Oct/2011 Steve Jobs, Apple’s Visionary, Dies at 56";
    //const CScript genesisOutputScript = CScript() << ParseHex("040184710fa689ad5023690c80f3a49c8f13f8d45b8c857fbcbc8bc4a8e4d3eb4b10f4d4604fa08dce601aaf0f470216fe1b51850b4acf21b179c45070ac7b03a9") << OP_CHECKSIG;
    val pszTimestamp = "NY Times 05/Oct/2011 Steve Jobs, Apple’s Visionary, Dies at 56"
    val asm = Seq(BytesToPushOntoStack(65),
      ScriptConstant("040184710fa689ad5023690c80f3a49c8f13f8d45b8c857fbcbc8bc4a8e4d3eb4b10f4d4604fa08dce601aaf0f470216fe1b51850b4acf21b179c45070ac7b03a9"),
      OP_CHECKSIG)
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    super.createGenesisBlock(pszTimestamp,genesisOutputScript,time,nonce,nBits,version,amount)
  }

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    PubKeyAddress -> UInt8(48).bytes,
    //TODO: investigate why litcoin has two script address formats
    //https://github.com/litecoin-project/litecoin/blob/master/src/chainparams.cpp#L134
    ScriptAddress -> UInt8(50).bytes,
    SecretKey -> Seq(1,176.toByte),
    //(0x04)(0x88)(0xB2)(0x1E)
    ExtPublicKey -> Seq(h2b("04"), h2b("88"), h2b("b2"), h2b("1e")),
    //(0x04)(0x88)(0xAD)(0xE4)
    ExtSecretKey -> Seq(h2b("04"), h2b("88"), h2b("ad"), h2b("e4"))
  )

  override def requireStandardTransaction: Boolean = true

}

sealed abstract class Base58Type
case object PubKeyAddress extends Base58Type
case object ScriptAddress extends Base58Type
case object SecretKey extends Base58Type
case object ExtPublicKey extends Base58Type
case object ExtSecretKey extends Base58Type