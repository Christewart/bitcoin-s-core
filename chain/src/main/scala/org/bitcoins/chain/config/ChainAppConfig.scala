package org.bitcoins.chain.config

import com.typesafe.config.{Config, ConfigException}
import org.bitcoins.chain.ChainCallbacks
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.pow.Pow
import org.bitcoins.commons.config.AppConfigFactory
import org.bitcoins.core.api.CallbackConfig
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.db._

import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S chain verification module
  * @param directory The data directory of the module
  * @param confs Optional sequence of configuration overrides
  */
case class ChainAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit override val ec: ExecutionContext)
    extends DbAppConfig
    with ChainDbManagement
    with JdbcProfileComponent[ChainAppConfig]
    with CallbackConfig[ChainCallbacks] {

  override protected[bitcoins] def moduleName: String =
    ChainAppConfig.moduleName
  override protected[bitcoins] type ConfigType = ChainAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Vector[Config]): ChainAppConfig =
    ChainAppConfig(baseDatadir, configs)

  override lazy val appConfig: ChainAppConfig = this

  override lazy val callbackFactory: ChainCallbacks.type = ChainCallbacks

  /** Checks whether or not the chain project is initialized by
    * trying to read the genesis block header from our block
    * header table
    */
  def isStarted(): Future[Boolean] = {
    val bhDAO = BlockHeaderDAO()(ec, appConfig)
    val isDefinedOptF = {
      bhDAO.read(chain.genesisBlock.blockHeader.hashBE).map(_.isDefined)
    }
    isDefinedOptF.foreach { _ =>
      logger.debug(s"Chain project is initialized")
    }
    isDefinedOptF.recover { case _: Throwable =>
      logger.info(s"Chain project is not initialized")
      false
    }
  }

  /** Initializes our chain project if it is needed
    * This creates the necessary tables for the chain project
    * and inserts preliminary data like the genesis block header
    */
  override def start(): Future[Unit] = {
    for {
      _ <- super.start()
      numMigrations = migrate()
      isInit <- isStarted()
      _ <- {
        if (isInit) {
          Future.unit
        } else {
          val genesisHeader =
            BlockHeaderDbHelper.fromBlockHeader(
              height = 0,
              chainWork = Pow.getBlockProof(chain.genesisBlock.blockHeader),
              bh = chain.genesisBlock.blockHeader)

          val blockHeaderDAO = BlockHeaderDAO()(ec, appConfig)
          val bhCreatedF = blockHeaderDAO.create(genesisHeader)
          bhCreatedF.flatMap { _ =>
            logger.info(s"Inserted genesis block header into DB")
            Future.unit
          }
        }
      }
    } yield {
      if (isHikariLoggingEnabled) {
        //.get is safe because hikari logging is enabled
        startHikariLogger(hikariLoggingInterval.get)
        ()
      }

      logger.info(s"Applied ${numMigrations} to chain project")
      ()
    }
  }

  override def stop(): Future[Unit] = {
    val _ = stopHikariLogger()
    clearCallbacks()
    super.stop()
  }

  lazy val filterHeaderBatchSize: Int = {
    // try by network, if that fails, try general
    try {
      config.getInt(
        s"bitcoin-s.$moduleName.neutrino.filter-header-batch-size.${chain.network.chainParams.networkId}")
    } catch {
      case _: ConfigException.Missing | _: ConfigException.WrongType =>
        config.getInt(
          s"bitcoin-s.$moduleName.neutrino.filter-header-batch-size.default")
    }
  }

  lazy val filterBatchSize: Int =
    config.getInt(s"bitcoin-s.${moduleName}.neutrino.filter-batch-size")

  /** Whether we should emit block processed events during IBD or not.
    * This is because websocket events can overwhelm UIs during IBD.
    * If this is set, we won't emit blockprocessed event until ibd is complete.
    */
  lazy val ibdBlockProcessedEvents: Boolean = {
    config.getBoolean(s"bitcoin-s.${moduleName}.websocket.block-processed-ibd")
  }

}

object ChainAppConfig extends AppConfigFactory[ChainAppConfig] {

  override val moduleName: String = "chain"

  /** Constructs a chain verification configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): ChainAppConfig =
    ChainAppConfig(datadir, confs)
}
