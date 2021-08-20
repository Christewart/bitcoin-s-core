package org.bitcoins.bundle.gui

import akka.actor.ActorSystem
import com.typesafe.config._
import grizzled.slf4j.Logging
import org.bitcoins.bundle.gui.BundleGUI._
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.commons.util.{DatadirUtil, ServerArgParser}
import org.bitcoins.gui._
import org.bitcoins.node.NodeType._
import org.bitcoins.node._
import org.bitcoins.server.BitcoinSAppConfig.toNodeConf
import org.bitcoins.server._
import scalafx.beans.property.ObjectProperty
import scalafx.stage.Window

import java.nio.file.Files
import scala.concurrent._
import scala.concurrent.duration.DurationInt

class LandingPaneModel(serverArgParser: ServerArgParser)(implicit
    system: ActorSystem)
    extends Logging {

  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] = {
    ObjectProperty[Window](null.asInstanceOf[Window])
  }

  def launchWallet(bundleConf: Config, appConfig: BitcoinSAppConfig): Unit = {
    logger.error(s"-------------Calling launchWallet-------------")
    taskRunner.run(
      "Launching Wallet",
      op = {
        import system.dispatcher
        val file = appConfig.baseDatadir.resolve("bitcoin-s-bundle.conf")

        val bundleConfStr = AppConfig.configToString(bundleConf)

        logger.info(s"Writing bundle config to $file")
        Files.write(file, bundleConfStr.getBytes)

        val networkConfigF: Future[Config] = {
          logger.error(s"-------networkConfigF-----------")
          val tmpConf =
            BitcoinSAppConfig.fromConfig(
              bundleConf.withFallback(appConfig.config))
          val netConfF: Future[Config] = tmpConf.nodeType match {
            case _: InternalImplementationNodeType =>
              // If we are connecting to a node we cannot
              // know what network it is on now
              Future.successful(ConfigFactory.empty())
            case BitcoindBackend =>
              logger.info(s"Attempting to get the bitcoind networkConfig")
              if (!appConfig.torConf.enabled) {
                tmpConf.bitcoindRpcConf.client.getBlockChainInfo.map { info =>
                  val networkStr =
                    DatadirUtil.networkStrToDirName(info.chain.name)
                  ConfigFactory.parseString(s"bitcoin-s.network = $networkStr")
                }
              } else {
                //we cannot connect to bitcoind and determine
                //the network over tor since tor isn't started
                //yet
                Future.successful(ConfigFactory.empty())
              }

          }

          netConfF.failed.foreach(_ =>
            logger.error(s"Failed to get bitcoind network"))

          netConfF.map { netConf =>
            serverArgParser.toConfig
              .withFallback(netConf)
              .withFallback(bundleConf)
          }
        }

        // Launch wallet
        val promise = Promise[Unit]()
        BitcoinSServer.startedF.map { _ =>
          fetchStartingData()
          changeToWalletGUIScene()
          promise.success(())
        }

        val startedF = networkConfigF.map { networkConfig =>
          logger.error(s"Creating finalAppConfig")
          val finalAppConfig =
            BitcoinSAppConfig.fromDatadir(appConfig.baseDatadir, networkConfig)
          // use class base constructor to share the actor system

          GlobalData.setBitcoinNetwork(
            finalAppConfig.network,
            finalAppConfig.socks5ProxyParams.isDefined)
          logger.error(
            s"------------------BEFORE STARTING BITCOINSERVERMAIN------------------")
          new BitcoinSServerMain(serverArgParser)(system, finalAppConfig)
            .run()
        }

        startedF.failed.foreach { case err =>
          throw err
        }

        Await.result(promise.future, 120.seconds)
      }
    )
  }
}
