package org.bitcoins.node.networking

import akka.io.Tcp
import akka.testkit.{TestActorRef, TestProbe}
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.node.SpvNodeCallbacks
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiver
import org.bitcoins.node.networking.peer.PeerMessageReceiverState.Preconnection
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest._

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/**
  * Created by chris on 6/7/16.
  */
class ClientTest
    extends BitcoindRpcTest
    with MustMatchers
    with BeforeAndAfter
    with BeforeAndAfterAll {

  implicit private val config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getTestConfig()
  implicit private val chainConf = config.chainConf
  implicit private val nodeConf = config.nodeConf
  implicit private val timeout = akka.util.Timeout(10.seconds)

  implicit val np = config.chainConf.network

  lazy val bitcoindRpcF =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeerF = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  lazy val bitcoindRpc2F =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeer2F = bitcoindRpcF.map { bitcoind =>
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  override def beforeAll(): Unit = {
    ChainDbManagement.createHeaderTable()
  }

  override def afterAll(): Unit = {
    ChainDbManagement.dropHeaderTable()
    super.afterAll()
  }

  behavior of "Client"

  it must "establish a tcp connection with a bitcoin node" in {
    bitcoindPeerF.flatMap(remote => connectAndDisconnect(remote))
  }

  it must "connect to two nodes" in {
    val try1 =
      bitcoindPeerF.flatMap(remote => connectAndDisconnect(remote))

    val try2 = bitcoindPeer2F.flatMap(remote => connectAndDisconnect(remote))

    try1.flatMap { _ =>
      try2
    }
  }

  /**
    * Helper method to connect to the
    * remote node and bind our local
    * connection to the specified port
    * @param remote the remote node on the p2p network we are connecting to
    * @param port the port we are binding on our machine
    * @return
    */
  def connectAndDisconnect(peer: Peer): Future[Assertion] = {
    val probe = TestProbe()
    val remote = peer.socket
    val peerMessageReceiverF =
      PeerMessageReceiver.preConnection(peer, SpvNodeCallbacks.empty)
    val clientActorF: Future[TestActorRef[P2PClientActor]] =
      peerMessageReceiverF.map { peerMsgRecv =>
        TestActorRef(P2PClient.props(peer, peerMsgRecv), probe.ref)
      }

    val p2pClientF: Future[P2PClient] = clientActorF.map {
      client: TestActorRef[P2PClientActor] =>
        P2PClient(client, peer)
    }

    val isConnectedF = for {
      p2pClient <- p2pClientF
      _ <- p2pClientF.map(p2pClient => p2pClient.actor ! Tcp.Connect(remote))
      isConnected <- TestAsyncUtil.retryUntilSatisfiedF(p2pClient.isConnected)
    } yield isConnected

    isConnectedF.flatMap { _ =>
      val isDisconnectedF = for {
        p2pClient <- p2pClientF
        _ = p2pClient.actor ! Tcp.Abort
        isDisconnected <- TestAsyncUtil.retryUntilSatisfiedF(
          p2pClient.isDisconnected,
          duration = 1.seconds)
      } yield isDisconnected

      isDisconnectedF.map { _ =>
        succeed
      }
    }
  }

}
