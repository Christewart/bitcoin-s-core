package org.bitcoins.testkit.node

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.{
  PeerHandler,
  PeerMessageReceiver,
  PeerMessageReceiverState,
  PeerMessageSender
}
import org.bitcoins.node.{SpvNode, SpvNodeCallbacks}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.scalatest.{
  BeforeAndAfter,
  BeforeAndAfterAll,
  FutureOutcome,
  MustMatchers
}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest
    extends BitcoinSFixture
    with MustMatchers
    with BitcoinSLogger
    with BeforeAndAfter
    with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.nodeConf)
  }

  override def afterAll(): Unit = {
    system.terminate()
    ()
  }

  implicit lazy val system: ActorSystem = {
    ActorSystem(s"${getClass.getSimpleName}-${System.currentTimeMillis}")
  }

  implicit lazy val ec: ExecutionContext =
    system.dispatcher

  val timeout: FiniteDuration = 10.seconds

  /** Wallet config with data directory set to user temp directory */
  implicit protected lazy val config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getTestConfig()

  implicit lazy val np: NetworkParameters = config.nodeConf.network

  lazy val startedBitcoindF = BitcoindRpcTestUtil.startedBitcoindRpcClient()

  lazy val bitcoindPeerF = startedBitcoindF.map(NodeTestUtil.getBitcoindPeer)

  def buildPeerMessageReceiver(
      chainApi: ChainApi,
      peer: Peer): Future[PeerMessageReceiver] = {
    val receiver =
      PeerMessageReceiver(state = PeerMessageReceiverState.fresh(),
                          chainApi = chainApi,
                          peer = peer,
                          callbacks = SpvNodeCallbacks.empty)
    Future.successful(receiver)
  }

  def buildPeerHandler(): Future[PeerHandler] = {
    bitcoindPeerF.flatMap { peer =>
      val chainApiF = ChainUnitTest.createChainHandler()
      val peerMsgReceiverF = chainApiF.flatMap { _ =>
        PeerMessageReceiver.preConnection(peer, SpvNodeCallbacks.empty)
      }
      //the problem here is the 'self', this needs to be an ordinary peer message handler
      //that can handle the handshake
      val peerHandlerF = for {
        peerMsgReceiver <- peerMsgReceiverF
        client = NodeTestUtil.client(peer, peerMsgReceiver)
        peerMsgSender = PeerMessageSender(client)
      } yield PeerHandler(client, peerMsgSender)

      peerHandlerF
    }

  }

  def peerSocketAddress(
      bitcoindRpcClient: BitcoindRpcClient): InetSocketAddress = {
    NodeTestUtil.getBitcoindSocketAddress(bitcoindRpcClient)
  }

  def createPeer(bitcoind: BitcoindRpcClient): Peer = {
    val socket = peerSocketAddress(bitcoind)
    Peer(id = None, socket = socket)
  }

  def createSpvNode(bitcoind: BitcoindRpcClient): Future[SpvNode] = {
    val chainApiF = ChainUnitTest.createChainHandler()
    val peer = createPeer(bitcoind)
    chainApiF.map { _ =>
      SpvNode(peer = peer, bloomFilter = NodeTestUtil.emptyBloomFilter)
    }
  }

  def withSpvNode(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {

    val spvBuilder: () => Future[SpvNode] = { () =>
      val bitcoindF = createBitcoind()
      bitcoindF.flatMap { bitcoind =>
        createSpvNode(bitcoind).flatMap(_.start())
      }
    }

    makeDependentFixture(
      build = spvBuilder,
      destroy = NodeUnitTest.destroySpvNode
    )(test)
  }

  def withSpvNodeConnectedToBitcoind(test: OneArgAsyncTest)(
      implicit system: ActorSystem): FutureOutcome = {
    val spvWithBitcoindBuilder: () => Future[SpvNodeConnectedWithBitcoind] = {
      () =>
        val bitcoindF = createBitcoind()
        bitcoindF.flatMap { bitcoind =>
          val startedSpv = createSpvNode(bitcoind).flatMap(_.start())

          startedSpv.map(spv => SpvNodeConnectedWithBitcoind(spv, bitcoind))
        }
    }

    makeDependentFixture(
      build = spvWithBitcoindBuilder,
      destroy = NodeUnitTest.destorySpvNodeConnectedWithBitcoind
    )(test)
  }

}

object NodeUnitTest extends BitcoinSLogger {

  def destroySpvNode(spvNode: SpvNode)(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    val stopF = spvNode.stop()
    stopF.flatMap(_ => ChainUnitTest.destroyHeaderTable())
  }

  def destorySpvNodeConnectedWithBitcoind(
      spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind)(
      implicit system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    logger.debug(s"Beggining tear down of spv node connected with bitcoind")
    import system.dispatcher
    val spvNode = spvNodeConnectedWithBitcoind.spvNode
    val bitcoind = spvNodeConnectedWithBitcoind.bitcoind
    val spvNodeDestroyF = destroySpvNode(spvNode)
    val bitcoindDestroyF = ChainUnitTest.destroyBitcoind(bitcoind)

    for {
      _ <- spvNodeDestroyF
      _ <- bitcoindDestroyF
    } yield {
      logger.debug(s"Done with teardown of spv node connected with bitcoind!")
      ()
    }
  }
}
