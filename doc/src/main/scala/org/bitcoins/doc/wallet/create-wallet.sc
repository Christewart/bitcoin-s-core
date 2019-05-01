import java.io.File

import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.{Block, RegTestNetChainParams}
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.api.InitializeWalletSuccess
import scodec.bits.ByteVector
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.blockchain.sync.ChainSync
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.db.WalletDbConfig
import org.bitcoins.wallet.config.WalletAppConfig

import org.bitcoins.zmq.ZMQSubscriber
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.util._
/**
* This is for example purposes only!
  * This shows how to peer a bitcoin-s wallet
  * with a bitcoind instance that is relaying
  * information about what is happening on the blockchain
  * to the bitcoin-s wallet.
  *
  * This is useful if you want more flexible signing
  * procedures in the JVM ecosystem and more
  * granular control over your utxos with
  * popular databases like postgres, sqlite etc
  */


val logger = LoggerFactory.getLogger("org.bitcoins.doc.wallet.CreateWallet")
val time = System.currentTimeMillis()
//boiler plate config
implicit val system = ActorSystem(s"wallet-scala-sheet-${time}")
import system.dispatcher

val chainDbConfig = ChainDbConfig.RegTestDbConfig
val chainAppConfig = ChainAppConfig(chainDbConfig)
implicit val chainParams = chainAppConfig.chain

val walletDbConfig = WalletDbConfig.RegTestDbConfig
val walletAppConfig = WalletAppConfig(walletDbConfig)

val datadir = new File(s"/tmp/bitcoin-${time}/")
val bitcoinConf = new File(datadir.getAbsolutePath + "/bitcoin.conf")
datadir.mkdirs()
bitcoinConf.createNewFile()

val config = BitcoindRpcTestUtil.standardConfig
val _ = BitcoindRpcTestUtil.writeConfigToFile(config,datadir)

//construct bitcoind
val instance = BitcoindInstance.fromConfig(config = config, datadir)
val bitcoind = new BitcoindRpcClient(instance = instance)

//start bitcoind, this may take a little while
//generate 101 blocks so we have money in our wallet
val bitcoindF = bitcoind.start().map(_ => bitcoind)

//create a native chain handler for bitcoin-s
val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO(appConfig = chainAppConfig)
val genesisHeader = BlockHeaderDbHelper.fromBlockHeader(
  height = 0,
  bh = chainAppConfig.chain.genesisBlock.blockHeader)


val blockHeaderTableF = {
  //drop regtest table if it exists
  val dropTableF = ChainDbManagement.dropHeaderTable(chainDbConfig)

  //recreate the table
  val createdTableF = dropTableF.flatMap(_ => ChainDbManagement.createHeaderTable(chainDbConfig))

  createdTableF
}
val createdGenHeaderF = blockHeaderTableF.flatMap(_ => blockHeaderDAO.create(genesisHeader))

val chainF = createdGenHeaderF.map(h => Vector(h))

val blockchainF = chainF.map(chain => Blockchain(chain,blockHeaderDAO))

val chainHandlerF = blockchainF.map(blockchain => ChainHandler(blockchain))

//we need a way to connect bitcoin-s to our running bitcoind, we are going to do this via rpc for now
//we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
//to be able to sync our internal bitcoin-s chain with our external bitcoind chain
val getBestBlockHashFunc = { () =>
  bitcoindF.flatMap(_.getBestBlockHash)
}

val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
  bitcoindF.flatMap(_.getBlockHeader(hash).map(_.blockHeader))
}


//now that we have bitcoind setup correctly and have rpc linked to
//the bitcoin-s chain project, let's generate some blocks so
//we have money to spend in our bitcoind wallet!
//we need to generate 101 blocks to give us 50 btc to spend
val genBlocksF = chainHandlerF.flatMap { _ =>
  bitcoindF.flatMap(_.generate(101))
}

//now we need to sync those blocks into bitcoin-s
val chainSyncF = genBlocksF.flatMap { _ =>
  chainHandlerF.flatMap { ch =>
    ChainSync.sync(
      ch,
      getBlockHeaderFunc,
      getBestBlockHashFunc)
  }
}

val bitcoinsLogF = chainSyncF.map { chainApi =>
  chainApi.getBlockCount.map(count => logger.info(s"bitcoin-s blockcount=${count}"))
}

val walletF = bitcoinsLogF.flatMap { _ =>
  //create tables
  val createTablesF = WalletDbManagement.createAll(walletDbConfig)
  createTablesF.flatMap { _ =>
    Wallet.initialize(walletAppConfig)
      .map(_.asInstanceOf[InitializeWalletSuccess].wallet)
  }
}

val addressF = walletF.flatMap(_.getNewAddress())

//send money to our wallet with bitcoind

//clean everything up
addressF.onComplete { _ =>
  cleanup()
}



def cleanup(): Future[Unit] = {
  logger.info("Beginning clean up of create wallet script")
  val bitcoindStopF = {
    bitcoindF.flatMap { bitcoind =>
      val stopF = bitcoind.stop()
      stopF
    }
  }
  datadir.delete()
  logger.debug("cleaning up chain, wallet, and system")
  val chainCleanupF = ChainDbManagement.dropAll(chainDbConfig)
  val walletCleanupF = WalletDbManagement.dropAll(walletDbConfig)

  val doneWithCleanupF = for {
    _ <- bitcoindStopF
    _ <- chainCleanupF
    _ <- walletCleanupF
    _ <- system.terminate()
  } yield {
    logger.info(s"Done cleaning up")
  }

  doneWithCleanupF
}


