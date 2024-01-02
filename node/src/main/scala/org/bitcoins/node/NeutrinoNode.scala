package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable}
import akka.stream.scaladsl.{
  Keep,
  RunnableGraph,
  Source,
  SourceQueue,
  SourceQueueWithComplete
}
import akka.stream.{
  ActorAttributes,
  OverflowStrategy,
  QueueOfferResult,
  Supervision
}
import akka.Done
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.node.NodeState.DoneSyncing
import org.bitcoins.core.api.node.{NodeState, NodeType, Peer}
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import org.bitcoins.core.p2p.{
  GetDataMessage,
  Inventory,
  InventoryMessage,
  TypeIdentifier
}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.BroadcastAbleTransaction

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

case class NeutrinoNode(
    walletCreationTimeOpt: Option[Instant],
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem,
    paramPeers: Vector[Peer])
    extends Node
    with SourceQueue[NodeStreamMessage] {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node, got=${nodeConfig.nodeType}!")

  private val isStarted: AtomicBoolean = new AtomicBoolean(false)
  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override def chainAppConfig: ChainAppConfig = chainConfig

  private val dataMessageStreamSource: Source[
    NodeStreamMessage,
    SourceQueueWithComplete[NodeStreamMessage]] = {
    Source
      .queue[NodeStreamMessage](
        100 * nodeAppConfig.maxConnectedPeers,
        overflowStrategy = OverflowStrategy.backpressure,
        maxConcurrentOffers = Runtime.getRuntime.availableProcessors())
  }

  private lazy val peerFinder: PeerFinder = PeerFinder(paramPeers = paramPeers,
                                                       queue = this,
                                                       skipPeers =
                                                         () => Set.empty)

  lazy val peerManager: PeerManager = {
    PeerManager(paramPeers = paramPeers,
                walletCreationTimeOpt = walletCreationTimeOpt,
                queue = this,
                finder = peerFinder)
  }

  private[this] var queueOpt: Option[
    SourceQueueWithComplete[NodeStreamMessage]] =
    None

  private[this] var streamDoneFOpt: Option[Future[NodeState]] = None

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Error occurred while processing p2p pipeline stream", err)
    Supervision.Resume
  }

  private def buildStreamGraph(
      initState: NodeState,
      source: Source[
        NodeStreamMessage,
        SourceQueueWithComplete[NodeStreamMessage]]): RunnableGraph[
    (SourceQueueWithComplete[NodeStreamMessage], Future[NodeState])] = {

    val graph = source
      .toMat(peerManager.buildP2PMessageHandlerSink(initState))(Keep.both)
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
    graph
  }

  override def start(): Future[NeutrinoNode] = {
    logger.info("Starting NeutrinoNode")
    isStarted.set(true)
    val start = System.currentTimeMillis()
    val initState =
      DoneSyncing(peersWithServices = Set.empty,
                  waitingForDisconnection = Set.empty)

    val graph =
      buildStreamGraph(initState = initState, source = dataMessageStreamSource)
    val (queue, stateF) = graph.run()
    queueOpt = Some(queue)
    streamDoneFOpt = Some(stateF)
    val startedNodeF = for {
      _ <- peerManager.start()
      _ <- peerFinder.start()
      _ = {
        val inactivityCancellable = startInactivityChecksJob()
        inactivityCancellableOpt = Some(inactivityCancellable)
      }
    } yield {
      this
    }

    startedNodeF.failed.foreach(logger.error("Cannot start Neutrino node", _))

    val chainApiF = chainApiFromDb()

    val bestHashF = chainApiF.flatMap(_.getBestBlockHash())
    val bestHeightF = chainApiF.flatMap(_.getBestHashBlockHeight())
    val filterHeaderCountF = chainApiF.flatMap(_.getFilterHeaderCount())
    val filterCountF = chainApiF.flatMap(_.getFilterCount())

    for {
      node <- startedNodeF
      bestHash <- bestHashF
      bestHeight <- bestHeightF
      filterHeaderCount <- filterHeaderCountF
      filterCount <- filterCountF
    } yield {
      logger.info(
        s"Started node, best block hash ${bestHash.hex} at height $bestHeight, with $filterHeaderCount filter headers and $filterCount filters. It took=${System
          .currentTimeMillis() - start}ms")
      node
    }
  }

  override def stop(): Future[NeutrinoNode] = {
    logger.info(s"Stopping NeutrinoNode")
    isStarted.set(false)
    val start = System.currentTimeMillis()
    inactivityCancellableOpt.map(_.cancel())
    for {
      _ <- peerFinder.stop()
      _ <- peerManager.stop()
      _ = queueOpt.map(_.complete())
      _ <- {
        val finishedF = streamDoneFOpt match {
          case Some(f) => f
          case None    => Future.successful(Done)
        }
        finishedF
      }
      _ = {
        //reset all variables
        streamDoneFOpt = None
        inactivityCancellableOpt = None
        queueOpt = None
      }
    } yield {
      logger.info(
        s"Node stopped! It took=${System.currentTimeMillis() - start}ms")
      this
    }
  }

  /** Starts to sync our node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    *
    * @return
    */
  override def sync(): Future[Unit] = {
    //wait for a peer to be available to sync from...
    //due to underlying mutability in PeerManager/PeerFinder
    //we may not have a peer available for selection immediately
    val peerAvailableF =
      AsyncUtil.retryUntilSatisfiedF(() => getConnectionCount.map(_ > 0))
    for {
      _ <- peerAvailableF
      _ <- peerManager.sync(None)
    } yield ()
  }

  /** Gets the number of compact filters in the database */
  override def getFilterCount(): Future[Int] =
    chainApiFromDb().flatMap(_.getFilterCount())

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    chainApiFromDb().flatMap(_.getHeightByBlockStamp(blockStamp))

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]] =
    chainApiFromDb().flatMap(_.getFiltersBetweenHeights(startHeight, endHeight))

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransactions(
      transactions: Vector[Transaction]): Future[Unit] = {
    val broadcastTxDbs = transactions.map(tx => BroadcastAbleTransaction(tx))

    val addToDbF = txDAO.upsertAll(broadcastTxDbs)

    val txIds = transactions.map(_.txIdBE.hex)

    addToDbF.onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TXs to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.map(_.transaction.txIdBE.hex)} to broadcastable table")
    }

    for {
      _ <- addToDbF
      _ <- {
        val connected = peerManager.peers.nonEmpty
        if (connected) {
          logger.info(s"Sending out tx message for tx=$txIds")
          val inventories =
            transactions.map(t => Inventory(TypeIdentifier.MsgTx, t.txId))
          val invMsg = InventoryMessage(inventories)
          peerManager.sendToRandomPeer(invMsg)
        } else {
          Future.failed(
            new RuntimeException(
              s"Error broadcasting transaction $txIds, no peers connected"))
        }
      }
    } yield ()
  }

  /** Fetches the given blocks from the peers and calls the appropriate [[callbacks]] when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    if (blockHashes.isEmpty) {
      Future.unit
    } else {
      val typeIdentifier = TypeIdentifier.MsgWitnessBlock
      val inventories =
        blockHashes.map(hash => Inventory(typeIdentifier, hash))
      val message = GetDataMessage(inventories)
      for {
        _ <- peerManager.sendToRandomPeer(message)
      } yield ()
    }
  }

  override def getConnectionCount: Future[Int] = {
    Future.successful(peerManager.connectedPeerCount)
  }

  private[this] val INACTIVITY_CHECK_TIMEOUT = 60.seconds

  @volatile private[this] var inactivityCancellableOpt: Option[Cancellable] =
    None

  private def inactivityChecksRunnable(): Runnable = { () =>
    val peers = peerManager.peers
    logger.debug(s"Running inactivity checks for peers=${peers}")
    val resultF = if (peers.nonEmpty) {
      Future.unit //do nothing?
    } else if (isStarted.get) {
      //stop and restart to get more peers
      stop()
        .flatMap(_.start())
        .map(_ => ())
    } else {
      start().map(_ => ())
    }

    resultF.failed.foreach(err =>
      logger.error(s"Failed to run inactivity checks for peers=${peers}", err))

    Await.result(resultF, INACTIVITY_CHECK_TIMEOUT)
  }

  private def startInactivityChecksJob(): Cancellable = {
    //the interval is set shorter for some unit test cases
    val interval = nodeAppConfig.network match {
      case MainNet | TestNet3 | SigNet => 5.minute
      case RegTest                     => nodeAppConfig.inactivityTimeout
    }
    system.scheduler.scheduleAtFixedRate(
      initialDelay = interval,
      interval = interval)(inactivityChecksRunnable())
  }

  override def offer(elem: NodeStreamMessage): Future[QueueOfferResult] = {
    queueOpt match {
      case Some(queue) => queue.offer(elem)
      case None =>
        Future.failed(new RuntimeException(
          s"NeutrinoNode not started, cannot process p2p message until NeutrinoNode.start() is called"))
    }
  }

  override def watchCompletion(): Future[Done] = {
    queueOpt match {
      case Some(queue) => queue.watchCompletion()
      case None        => Future.successful(Done)
    }
  }
}
