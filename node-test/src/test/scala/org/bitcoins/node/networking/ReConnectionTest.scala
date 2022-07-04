package org.bitcoins.node.networking

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.asyncutil.AsyncUtil.RpcRetryException
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerHandler
import org.bitcoins.testkit.node.{
  CachedBitcoinSAppConfig,
  NodeTestUtil,
  NodeUnitTest
}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.{AkkaUtil, BitcoindRpcTest}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class ReConnectionTest extends BitcoindRpcTest with CachedBitcoinSAppConfig {

  lazy val bitcoindRpcF =
    BitcoindRpcTestUtil.startedBitcoindRpcClient(clientAccum = clientAccum)

  lazy val bitcoindPeerF: Future[Peer] =
    bitcoindRpcF.flatMap(b => NodeTestUtil.getBitcoindPeer(b))

  behavior of "ReConnectionTest"

  it must "attempt to reconnect if max connections are full" in {
    logger.info(s"RECONNECTION TEST: started")
    val peerHandlerF: Future[PeerHandler] = for {
      _ <- cachedConfig.start()
      peer <- bitcoindPeerF.flatMap(p => NodeUnitTest.buildPeerHandler(p, None))
    } yield peer

    val connectedF = for {
      peer <- bitcoindPeerF
      _ = logger.info(s"RECONNECTION TEST: using $peer")
      peerHandler <- peerHandlerF
      _ = logger.info(s"RECONNECTION TEST: got peer handler")
      bitcoindRpc <- bitcoindRpcF
      _ = logger.info(s"RECONNECTION TEST: got bitcoindrpc")

      _ = peerHandler.peerMsgSender.connect()
      _ = logger.info(s"RECONNECTION TEST: sent connect")
      _ <- AsyncUtil
        .retryUntilSatisfiedF(() => peerHandler.p2pClient.isConnected())
        .recover { case _: RpcRetryException =>
          //expect this to fail, we cannot connect
          //because maxconnections=0
          ()
        }
      _ = logger.info(s"RECONNECTION TEST: stopping bitcoind")
      _ <- bitcoindRpc.stop()
      _ = logger.info(s"RECONNECTION TEST: stopped bitcoind")
      //need to wait for mac to unlock the datadir
      //before we can restart the bitcoind binary
      _ <- AkkaUtil.nonBlockingSleep(3.seconds)
      _ = logger.info(s"RECONNECTION TEST: starting bitcoind")
      _ <- bitcoindRpc.start()
      _ = logger.info(s"RECONNECTION TEST: started bitcoind")
      //now we should eventually automatically reconnect
      _ <- AsyncUtil.retryUntilSatisfiedF(
        conditionF = () => peerHandler.p2pClient.isConnected(),
        interval = 500.millis,
        maxTries = 60)
    } yield {
      logger.info(s"RECONNECTION TEST: completed test")
      succeed
    }

    connectedF
  }

  override def afterAll(): Unit = {
    super[CachedBitcoinSAppConfig].afterAll()
    super[BitcoindRpcTest].afterAll()
  }
}
