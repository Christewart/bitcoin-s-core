package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageSender
}
import org.bitcoins.rpc.util.AsyncUtil

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.p2p.FilterLoadMessage
import org.bitcoins.core.p2p.NetworkPayload
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.node.models.BroadcastAbleTransaction
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import slick.jdbc.SQLiteProfile

import scala.util.Failure
import scala.util.Success

case class SpvNode(
    peer: Peer,
    bloomFilter: BloomFilter,
    callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty
)(
    implicit system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends BitcoinSLogger {
  import system.dispatcher

  implicit private val timeout = akka.util.Timeout(10.seconds)
  private val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  def chainApiF: Future[ChainApi] = {
    ChainHandler.fromDatabase(BlockHeaderDAO(), chainAppConfig)
  }
  private val clientF: Future[P2PClient] = {
    for {
      chainApi <- chainApiF
    } yield {
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(chainApi = chainApi,
                                        peer = peer,
                                        callbacks = callbacks)
      val p2p = P2PClient(context = system,
                          peer = peer,
                          peerMessageReceiver = peerMsgRecv)
      p2p
    }
  }

  private val peerMsgSenderF: Future[PeerMessageSender] = {
    clientF.map { client =>
      PeerMessageSender(client)
    }
  }

  /**
    * Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  private[node] def send(msg: NetworkPayload): Future[Unit] = {
    peerMsgSenderF.map(_.sendMsg(msg))
  }

  /** Starts our spv node */
  def start(): Future[SpvNode] = {
    for {
      _ <- nodeAppConfig.initialize()
      node <- {
        val connectedF = peerMsgSenderF.map(_.connect())

        val isInitializedF = for {
          _ <- connectedF
          _ <- AsyncUtil.retryUntilSatisfiedF(() => isInitialized)
        } yield ()

        isInitializedF.failed.foreach(err =>
          logger.error(s"Failed to connect with peer=$peer with err=${err}"))

        isInitializedF.map { _ =>
          logger.info(s"Our peer=${peer} has been initialized")
          this
        }
      }
    } yield {
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peer")
      val filterMsg = FilterLoadMessage(bloomFilter)
      val _ = send(filterMsg)
      node
    }
  }

  /** Stops our spv node */
  def stop(): Future[SpvNode] = {
    val disconnectF = peerMsgSenderF.map(_.disconnect())

    val isStoppedF = disconnectF.flatMap { _ =>
      logger.info(s"Awaiting disconnect")
      AsyncUtil.retryUntilSatisfiedF(() => isDisconnected)
    }

    isStoppedF.map { _ =>
      logger.info(s"Spv node stopped!")
      this
    }
  }

  /** Broadcasts the given transaction over the P2P network */
  def broadcastTransaction(transaction: Transaction): Future[Unit] = {
    val broadcastTx = BroadcastAbleTransaction(transaction)

    txDAO.create(broadcastTx).onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TX to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.transaction.txIdBE} to broadcastable table")
    }

    logger.info(s"Sending out inv for tx=${transaction.txIdBE}")
    peerMsgSenderF.map(_.sendInventoryMessage(transaction))
  }

  /** Checks if we have a tcp connection with our peer */
  def isConnected: Future[Boolean] = clientF.flatMap(_.isConnected)

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    * @return
    */
  def isInitialized: Future[Boolean] = clientF.flatMap(_.isInitialized)

  def isDisconnected: Future[Boolean] = clientF.flatMap(_.isDisconnected)

  /** Starts to sync our spv node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    * @return
    */
  def sync(): Future[Unit] = {
    for {
      chainApi <- chainApiF
      hash <- chainApi.getBestBlockHash
      header <- chainApi
        .getHeader(hash)
        .map(_.get) // .get is safe since this is an internal call
    } yield {
      peerMsgSenderF.map(_.sendGetHeadersMessage(hash.flip))
      logger.info(s"Starting sync node, height=${header.height} hash=$hash")
    }
  }
}
