package org.bitcoins.server

import akka.actor.ActorSystem
import akka.stream.{KillSwitches, SharedKillSwitch}
import grizzled.slf4j.Logging
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.node.models.NodeStateDescriptorDAO
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.wallet.WalletHolder
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future}

/** A trait used to help load a different load and discard the current wallet in memory
  * This trait encapsulates the heavy lifting done in the 'loadwallet' RPC command
  */
sealed trait DLCWalletLoaderApi extends Logging {

  protected def conf: BitcoinSAppConfig

  private[this] var currentNodeCallbackKillSwitchOpt: Option[
    SharedKillSwitch] = {
    None
  }

  protected def getNodeCallbackKillSwitch: SharedKillSwitch = {
    currentNodeCallbackKillSwitchOpt match {
      case Some(ks) => ks
      case None =>
        sys.error(
          "No killswitch registered for node callbacks, this means loadWallet() was never called")
    }
  }

  def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)]

  protected def loadWallet(
      walletHolder: WalletHolder,
      chainQueryApi: ChainQueryApi,
      nodeApi: NodeApi,
      feeProviderApi: FeeRateApi,
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword])(implicit
      ec: ExecutionContext): Future[
    (AnyDLCHDWalletApi, WalletAppConfig, DLCAppConfig)] = {

    //kill previous callbacks
    currentNodeCallbackKillSwitchOpt match {
      case Some(callbackKillSwitches) =>
        callbackKillSwitches.abort(new RuntimeException("loadwallet switch"))
      case None =>
      //must be the first wallet we have loaded, so do nothing
    }

    //set our variable to the current killswitch
    currentNodeCallbackKillSwitchOpt = Some(
      KillSwitches.shared(
        s"nodecallback-killswitch-${System.currentTimeMillis()}"))

    logger.info(
      s"Loading wallet with bitcoind backend, walletName=${walletNameOpt.getOrElse("DEFAULT")}")

    val walletName =
      walletNameOpt.getOrElse(WalletAppConfig.DEFAULT_WALLET_NAME)

    for {
      (walletConfig, dlcConfig) <- updateWalletConfigs(walletName,
                                                       Some(aesPasswordOpt))
        .recover { case _: Throwable => (conf.walletConf, conf.dlcConf) }
      _ <- {
        if (walletHolder.isInitialized) {
          walletHolder
            .stop()
            .map(_ => ())
        } else {
          Future.unit
        }
      }
      _ <- walletConfig.start()
      _ <- dlcConfig.start()
      dlcWallet <- dlcConfig.createDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi,
        feeRateApi = feeProviderApi
      )(walletConfig, ec)
    } yield (dlcWallet, walletConfig, dlcConfig)
  }

  protected def updateWalletConfigs(
      walletName: String,
      aesPasswordOpt: Option[Option[AesPassword]])(implicit
      ec: ExecutionContext): Future[(WalletAppConfig, DLCAppConfig)] = {
    val kmConfigF = Future.successful(
      conf.walletConf.kmConf.copy(walletNameOverride = Some(walletName),
                                  aesPasswordOverride = aesPasswordOpt))

    (for {
      kmConfig <- kmConfigF
      _ = if (!kmConfig.seedExists())
        throw new RuntimeException(s"Wallet `${walletName}` does not exist")

      // First thing start the key manager to be able to fail fast if the password is invalid
      _ <- kmConfig.start()

      walletConfig = conf.walletConf.copy(kmConfOpt = Some(kmConfig))
      dlcConfig = conf.dlcConf.copy(walletConfigOpt = Some(walletConfig))
    } yield (walletConfig, dlcConfig))
  }

  protected def updateWalletName(walletNameOpt: Option[String])(implicit
      ec: ExecutionContext): Future[Unit] = {
    val nodeStateDAO: NodeStateDescriptorDAO =
      NodeStateDescriptorDAO()(ec, conf.nodeConf)
    nodeStateDAO.updateWalletName(walletNameOpt)
  }
}

case class DLCWalletNeutrinoBackendLoader(
    walletHolder: WalletHolder,
    chainQueryApi: ChainQueryApi,
    nodeApi: NodeApi,
    feeProvider: FeeRateApi)(implicit
    override val conf: BitcoinSAppConfig,
    system: ActorSystem)
    extends DLCWalletLoaderApi {
  import system.dispatcher
  implicit private val nodeConf = conf.nodeConf

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {

    val nodeApi = walletHolder.nodeApi
    val chainQueryApi = walletHolder.chainQueryApi
    val feeRateApi = walletHolder.feeRateApi
    for {
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = chainQueryApi,
        nodeApi = nodeApi,
        feeProviderApi = feeRateApi,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt
      )
      nodeCallbacks <-
        CallbackUtil.createNeutrinoNodeCallbacksForWallet(
          walletHolder,
          getNodeCallbackKillSwitch)
      _ = nodeConf.replaceCallbacks(nodeCallbacks)
      _ <- walletHolder.replaceWallet(dlcWallet)
      _ <- updateWalletName(walletNameOpt)
    } yield (walletHolder, walletConfig, dlcConfig)
  }
}

case class DLCWalletBitcoindBackendLoader(
    walletHolder: WalletHolder,
    bitcoind: BitcoindRpcClient,
    nodeApi: NodeApi,
    feeProvider: FeeRateApi)(implicit
    override val conf: BitcoinSAppConfig,
    system: ActorSystem)
    extends DLCWalletLoaderApi {
  import system.dispatcher
  implicit private val nodeConf = conf.nodeConf

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    for {
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = bitcoind,
        nodeApi = nodeApi,
        feeProviderApi = feeProvider,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt)

      nodeCallbacks <- CallbackUtil.createBitcoindNodeCallbacksForWallet(
        walletHolder,
        getNodeCallbackKillSwitch)
      _ = nodeConf.addCallbacks(nodeCallbacks)
      _ <- walletHolder.replaceWallet(dlcWallet)
    } yield (walletHolder, walletConfig, dlcConfig)
  }
}
