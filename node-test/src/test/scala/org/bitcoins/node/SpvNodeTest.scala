package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.node.NodeUnitTest
import org.bitcoins.testkit.node.fixture.SpvNodeConnectedWithBitcoind
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class SpvNodeTest extends NodeUnitTest {

  override type FixtureParam = SpvNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withSpvNodeConnectedToBitcoind(test)

  behavior of "SpvNode"

  override implicit val system: ActorSystem = ActorSystem(
    s"SpvNodeTest-${System.currentTimeMillis}")

  it must "receive notification that a block occurred on the p2p network" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.spvNode
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      assert(spvNode.isConnected)

      assert(spvNode.isInitialized)

      val hashF: Future[DoubleSha256DigestBE] = {
        bitcoind.generate(1).map(_.head)
      }

      //sync our spv node expecting to get that generated hash
      val spvSyncF = for {
        _ <- hashF
        sync <- spvNode.sync()
      } yield sync

      def isSameBestHash(): Future[Boolean] = {
        for {
          spvBestHash <- spvNode.chainApi.getBestBlockHash
          hash <- hashF
        } yield spvBestHash == hash
      }

      spvSyncF.flatMap { _ =>
        RpcUtil
          .retryUntilSatisfiedF(isSameBestHash)
          .map(_ => succeed)
      }
  }

  it must "stay in sync with a bitcoind instance" in {
    spvNodeConnectedWithBitcoind: SpvNodeConnectedWithBitcoind =>
      val spvNode = spvNodeConnectedWithBitcoind.spvNode
      val bitcoind = spvNodeConnectedWithBitcoind.bitcoind

      //this needs to be called to get our peer to send us headers
      //as they happen with the 'sendheaders' message
      //both our spv node and our bitcoind node _should_ both be at the genesis block (regtest)
      //at this point so no actual syncing is happening
      val initSyncF = spvNode.sync()

      //start generating a block every 10 seconds with bitcoind
      //this should result in 5 blocks
      val startGenF = initSyncF.map { _ =>
        Thread.sleep(5000)
        //generate a block every 5 seconds
        //until we have generated 5 total blocks
        genBlock5seconds(bitcoind)
      }

      startGenF.flatMap { _ =>
        Thread.sleep(40000)

        //we should expect 5 headers have been announced to us via
        //the send headers message.
        spvNode.chainApi.getBlockCount.map(count => assert(count == 5))
      }
  }

  private var counter = 0
  private def genBlock5seconds(bitcoind: BitcoindRpcClient)(
      implicit system: ActorSystem): Unit = {
    val genBlock = new Runnable {
      override def run(): Unit = {
        if (counter < 5) {
          bitcoind.generate(1)
          counter = counter + 1
        } else {
          //do nothing
        }
      }
    }
    system.scheduler.schedule(5.seconds, 5.seconds, genBlock)
  }
}
