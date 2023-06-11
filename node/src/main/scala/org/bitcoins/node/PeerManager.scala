package org.bitcoins.node

import akka.{Done, NotUsed}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.stream.{
  ActorAttributes,
  OverflowStrategy,
  QueueOfferResult,
  Supervision
}
import akka.stream.scaladsl.{
  Keep,
  RunnableGraph,
  Sink,
  Source,
  SourceQueue,
  SourceQueueWithComplete
}
import grizzled.slf4j.Logging
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{FutureUtil, NetworkUtil, StartStopAsync}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.P2PClientSupervisor
import org.bitcoins.node.networking.peer.DataMessageHandlerState._
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.util.{BitcoinSNodeUtil, PeerMessageSenderApi}
import scodec.bits.ByteVector

import java.net.InetAddress
import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

case class PeerManager(
    paramPeers: Vector[Peer] = Vector.empty,
    walletCreationTimeOpt: Option[Instant])(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends StartStopAsync[PeerManager]
    with PeerMessageSenderApi
    with SourceQueue[StreamDataMessageWrapper]
    with P2PLogger {
  private val isStarted: AtomicBoolean = new AtomicBoolean(false)
  private val _peerDataMap: mutable.Map[Peer, PeerData] = mutable.Map.empty

  /** holds peers removed from peerData whose client actors are not stopped yet. Used for runtime sanity checks. */
  private val _waitingForDeletion: mutable.Set[Peer] = mutable.Set.empty
  def waitingForDeletion: Set[Peer] = _waitingForDeletion.toSet

  private val supervisor: ActorRef =
    system.actorOf(Props[P2PClientSupervisor](),
                   name =
                     BitcoinSNodeUtil.createActorName("P2PClientSupervisor"))

  private var finderOpt: Option[PeerFinder] = {
    None
  }

  def addPeerToTry(peers: Vector[Peer], priority: Int = 0): Unit = {
    finderOpt match {
      case Some(finder) => finder.addToTry(peers, priority)
      case None =>
        sys.error(
          s"Cannot addPeerToTry, finder not started. Call PeerManager.start()")
    }
  }

  def connectedPeerCount: Int = _peerDataMap.size

  private def addPeer(peer: Peer): Future[Unit] = {
    finderOpt match {
      case Some(finder) =>
        require(finder.hasPeer(peer), s"Unknown $peer marked as usable")
        val curPeerData = finder.popFromCache(peer).get
        _peerDataMap.put(peer, curPeerData)
        val hasCf =
          if (curPeerData.serviceIdentifier.nodeCompactFilters) "with filters"
          else ""
        logger.info(
          s"Connected to peer $peer $hasCf. Connected peer count $connectedPeerCount")
        Future.unit
      case None =>
        sys.error(
          s"Cannot addPeer, finder not started. Call PeerManager.start()")
    }
  }

  def peers: Vector[Peer] = _peerDataMap.keys.toVector

  override def sendMsg(
      msg: NetworkPayload,
      peerOpt: Option[Peer]): Future[Unit] = {
    val networkMessage = NetworkMessage(nodeAppConfig.network, msg)
    offer(SendToPeer(msg = networkMessage, peerOpt = peerOpt))
      .map(_ => ())
  }

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  override def gossipMessage(
      msg: NetworkPayload,
      excludedPeerOpt: Option[Peer]): Future[Unit] = {
    val gossipPeers = excludedPeerOpt match {
      case Some(excludedPeer) =>
        peerDataMap
          .filterNot(_._1 == excludedPeer)
          .map(_._1)
      case None => peerDataMap.map(_._1)
    }

    Future
      .traverse(gossipPeers)(p => sendMsg(msg, Some(p)))
      .map(_ => ())
  }

  override def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit] = {
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101).map(_.flip))
    sendMsg(headersMsg, peerOpt)
  }

  override def gossipGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit] = {
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101).map(_.flip))
    gossipMessage(headersMsg, None)
  }

  override def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit] = {
    val msg: NetworkPayload = {
      val inventories =
        hashes.map(hash => Inventory(typeIdentifier, hash.flip))
      val message = GetDataMessage(inventories)
      message
    }

    sendMsg(msg, peerOpt)
  }

  override def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker,
      peerOpt: Option[Peer]): Future[Unit] = {
    val message =
      GetCompactFilterHeadersMessage(if (filterSyncMarker.startHeight < 0) 0
                                     else filterSyncMarker.startHeight,
                                     filterSyncMarker.stopBlockHash)
    sendMsg(message, peerOpt)
  }

  override def sendGetCompactFiltersMessage(
      filterSyncMarker: FilterSyncMarker,
      peer: Peer)(implicit
      ec: ExecutionContext): Future[DataMessageHandlerState.FilterSync] = {
    val message =
      GetCompactFiltersMessage(if (filterSyncMarker.startHeight < 0) 0
                               else filterSyncMarker.startHeight,
                               filterSyncMarker.stopBlockHash)
    logger.debug(s"Sending getcfilters=$message to peer ${peer}")
    sendMsg(message, Some(peer)).map(_ =>
      DataMessageHandlerState.FilterSync(peer))
  }

  override def sendInventoryMessage(
      transactions: Vector[Transaction],
      peerOpt: Option[Peer]): Future[Unit] = {
    val inventories =
      transactions.map(tx => Inventory(TypeIdentifier.MsgTx, tx.txId))
    val message = InventoryMessage(inventories)
    logger.trace(s"Sending inv=$message to peer=${peerOpt}")
    peerOpt match {
      case Some(_) =>
        sendMsg(message, peerOpt)
      case None =>
        gossipMessage(msg = message, excludedPeerOpt = None)
    }
  }

  /** Starts sync compact filer headers.
    * Only starts syncing compact filters if our compact filter headers are in sync with block headers
    */
  def syncCompactFilters(
      bestFilterHeader: CompactFilterHeaderDb,
      chainApi: ChainApi,
      bestFilterOpt: Option[CompactFilterDb],
      dmhState: DataMessageHandlerState)(implicit
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val syncPeerOptF = {
      dmhState match {
        case syncState: SyncDataMessageHandlerState =>
          Some(syncState.syncPeer)
        case DoneSyncing | _: MisbehavingPeer | _: RemovePeers => None
      }
    }
    val sendCompactFilterHeaderMsgF = syncPeerOptF match {
      case Some(syncPeer) =>
        PeerManager.sendNextGetCompactFilterHeadersCommand(
          peerMessageSenderApi = this,
          chainApi = chainApi,
          peer = syncPeer,
          filterHeaderBatchSize = chainAppConfig.filterHeaderBatchSize,
          prevStopHash = bestFilterHeader.blockHashBE
        )
      case None => Future.successful(false)
    }
    sendCompactFilterHeaderMsgF.flatMap { isSyncFilterHeaders =>
      // If we have started syncing filters
      if (
        !isSyncFilterHeaders &&
        bestFilterOpt.isDefined &&
        bestFilterOpt.get.hashBE != bestFilterHeader.filterHashBE
      ) {
        syncPeerOptF match {
          case Some(syncPeer) =>
            //means we are not syncing filter headers, and our filters are NOT
            //in sync with our compact filter headers
            PeerManager
              .sendNextGetCompactFilterCommand(
                peerMessageSenderApi = this,
                chainApi = chainApi,
                filterBatchSize = chainAppConfig.filterBatchSize,
                startHeight = bestFilterOpt.get.height,
                peer = syncPeer)
              .map(_ => ())
          case None =>
            logger.warn(
              s"Not syncing compact filters since we do not have a syncPeer set, bestFilterOpt=$bestFilterOpt")
            Future.unit
        }
      } else {
        Future.unit
      }
    }
  }

  private def getPeerMsgSender(
      peer: Peer): Future[Option[PeerMessageSender]] = {
    _peerDataMap.find(_._1 == peer).map(_._2.peerMessageSender) match {
      case Some(peerMsgSender) => peerMsgSender.map(Some(_))
      case None                => Future.successful(None)
    }
  }

  def randomPeerWithService(
      services: ServiceIdentifier): Future[Option[Peer]] = {
    val filteredPeers =
      peerDataMap
        .filter(p => p._2.serviceIdentifier.hasServicesOf(services))
        .keys
        .toVector
    val (good, _) =
      filteredPeers.partition(p => !peerDataMap(p).hasFailedRecently)

    val peerOpt = if (good.nonEmpty) {
      Some(good(Random.nextInt(good.length)))
    } else {
      None
    }
    Future.successful(peerOpt)
  }

  private def randomPeerMsgSenderWithService(
      services: ServiceIdentifier): Future[Option[PeerMessageSender]] = {
    val randomPeerOptF = randomPeerWithService(services)
    randomPeerOptF.flatMap { peerOpt =>
      peerOpt match {
        case Some(peer) => peerDataMap(peer).peerMessageSender.map(Some(_))
        case None       => Future.successful(None)
      }
    }
  }

  private def createInDb(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Future[PeerDb] = {
    logger.debug(s"Adding peer to db $peer")
    val addrBytes =
      if (peer.socket.getHostString.contains(".onion"))
        NetworkUtil.torV3AddressToBytes(peer.socket.getHostString)
      else
        InetAddress.getByName(peer.socket.getHostString).getAddress
    val networkByte = addrBytes.length match {
      case AddrV2Message.IPV4_ADDR_LENGTH   => AddrV2Message.IPV4_NETWORK_BYTE
      case AddrV2Message.IPV6_ADDR_LENGTH   => AddrV2Message.IPV6_NETWORK_BYTE
      case AddrV2Message.TOR_V3_ADDR_LENGTH => AddrV2Message.TOR_V3_NETWORK_BYTE
      case unknownSize =>
        throw new IllegalArgumentException(
          s"Unsupported address type of size $unknownSize bytes")
    }
    PeerDAO()
      .upsertPeer(ByteVector(addrBytes),
                  peer.socket.getPort,
                  networkByte,
                  serviceIdentifier)
  }

  def replacePeer(replacePeer: Peer, withPeer: Peer): Future[Unit] = {
    logger.debug(s"Replacing $replacePeer with $withPeer")
    assert(!peerDataMap(replacePeer).serviceIdentifier.nodeCompactFilters,
           s"$replacePeer has cf")
    for {
      _ <- removePeer(replacePeer)
      _ <- addPeer(withPeer)
    } yield {
      ()
    }
  }

  def removePeer(peer: Peer): Future[Unit] = {
    logger.debug(s"Removing persistent peer $peer")
    val client: PeerData = peerDataMap(peer)
    _peerDataMap.remove(peer)
    //so we need to remove if from the map for connected peers so no more request could be sent to it but we before
    //the actor is stopped we don't delete it to ensure that no such case where peers is deleted but actor not stopped
    //leading to a memory leak may happen
    _waitingForDeletion.add(peer)
    //now send request to stop actor which will be completed some time in future
    client.stop()
  }

  def isReconnection(peer: Peer): Boolean = {
    peerDataMap.contains(peer)
  }

  override def start(): Future[PeerManager] = {
    logger.debug(s"Starting PeerManager")
    val (queue, source) = dataMessageStreamSource.preMaterialize()
    val initDmh = buildStatelessDataMessagehandler(queue)
    val graph = buildDataMessageStreamGraph(initDmh = initDmh, source = source)
    dataMessageQueueOpt = Some(queue)
    val dmhF = graph.run()
    streamDoneFOpt = Some(dmhF)
    val finder = PeerFinder(
      paramPeers = paramPeers,
      controlMessageHandler = ControlMessageHandler(this),
      queue = queue,
      peerMessageSenderApi = this,
      skipPeers = () => peers,
      supervisor = supervisor
    )
    finderOpt = Some(finder)
    finder.start().map { _ =>
      logger.info("Done starting PeerManager")
      isStarted.set(true)
      this
    }
  }

  private def peerDataMap: Map[Peer, PeerData] = _peerDataMap.toMap

  def getPeerData(peer: Peer): Option[PeerData] = peerDataMap.get(peer)

  override def stop(): Future[PeerManager] = {
    logger.info(s"Stopping PeerManager")
    isStarted.set(false)
    val beganAt = System.currentTimeMillis()

    val finderStopF = finderOpt match {
      case Some(finder) => finder.stop()
      case None         => Future.unit
    }

    val stopF = for {
      _ <- finderStopF
      _ <- Future.traverse(peers)(removePeer)
      _ <- AsyncUtil.retryUntilSatisfied(
        _peerDataMap.isEmpty && waitingForDeletion.isEmpty,
        interval = 1.seconds,
        maxTries = 30)
      _ = dataMessageQueueOpt.map(_.complete())
      _ <- {
        val finishedF = streamDoneFOpt match {
          case Some(f) => f
          case None    => Future.successful(Done)
        }
        finishedF
      }
      _ <- watchCompletion()
      _ = {
        //reset all variables
        dataMessageQueueOpt = None
        streamDoneFOpt = None
        finderOpt = None
      }
    } yield {
      logger.info(
        s"Stopped PeerManager. Took ${System.currentTimeMillis() - beganAt} ms ")
      this
    }

    stopF
  }

  def isConnected(peer: Peer): Future[Boolean] = {
    if (peerDataMap.contains(peer))
      peerDataMap(peer).peerMessageSender.flatMap(_.isConnected())
    else Future.successful(false)
  }

  def isDisconnected(peer: Peer): Future[Boolean] = {
    isConnected(peer).map(b => !b)
  }

  def isInitialized(peer: Peer): Future[Boolean] = {
    if (peerDataMap.contains(peer))
      peerDataMap(peer).peerMessageSender.flatMap(_.isInitialized())
    else Future.successful(false)
  }

  def onInitializationTimeout(peer: Peer): Future[Unit] = {
    finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        if (finder.hasPeer(peer)) {
          //one of the peers that we tried, failed to init within time, disconnect
          finder.getData(peer).get.stop()
        } else if (peerDataMap.contains(peer)) {
          //this is one of our persistent peers which must have been initialized earlier, this can happen in case of
          //a reconnection attempt, meaning it got connected but failed to initialize, disconnect
          peerDataMap(peer).stop()
        } else {
          //this should never happen
          logger.warn(s"onInitializationTimeout called for unknown $peer")
          Future.unit
        }
      case None =>
        logger.warn(
          s"Cannot execute onInitializationTimeout, finder not started")
        Future.unit
    }

  }

  private def onInitialization(peer: Peer): Future[Unit] = {
    finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        //this assumes neutrino and checks for compact filter support so should not be called for anything else
        require(nodeAppConfig.nodeType == NodeType.NeutrinoNode,
                s"Node cannot be ${nodeAppConfig.nodeType.shortName}")

        if (finder.hasPeer(peer)) {
          //one of the peers we tries got initialized successfully
          val peerData = finder.getData(peer).get
          val serviceIdentifer = peerData.serviceIdentifier
          val hasCf = serviceIdentifer.nodeCompactFilters
          logger.debug(s"Initialized peer $peer with $hasCf")

          def sendAddrReq: Future[Unit] = {
            sendGetAddrMessage(Some(peer))
          }

          def managePeerF(): Future[Unit] = {
            //if we have slots remaining, connect
            if (connectedPeerCount < nodeAppConfig.maxConnectedPeers) {
              addPeer(peer)
            } else {
              lazy val notCf = peerDataMap
                .filter(p => !p._2.serviceIdentifier.nodeCompactFilters)
                .keys

              //try to drop another non compact filter connection for this
              if (hasCf && notCf.nonEmpty)
                replacePeer(replacePeer = notCf.head, withPeer = peer)
              else {
                //no use for this apart from writing in db
                //we do want to give it enough time to send addr messages
                AsyncUtil
                  .nonBlockingSleep(duration = 10.seconds)
                  .flatMap { _ =>
                    //could have already been deleted in case of connection issues
                    finder.getData(peer) match {
                      case Some(p) => p.stop()
                      case None    => Future.unit
                    }
                  }
              }
            }
          }

          for {
            _ <- sendAddrReq
            _ <- createInDb(peer, peerData.serviceIdentifier)
            _ <- managePeerF()
          } yield ()

        } else if (peerDataMap.contains(peer)) {
          //one of the persistent peers initialized again, this can happen in case of a reconnection attempt
          //which succeeded which is all good, do nothing
          Future.unit
        } else {
          logger.warn(s"onInitialization called for unknown $peer")
          Future.unit
        }
      case None =>
        logger.warn(
          s"onInitialization cannot be run, PeerFinder was not started")
        Future.unit
    }

  }

  /** @param peer the peer we were disconencted from
    * @param reconnect flag indicating if we should attempt to reconnect
    * @return
    */
  private def onP2PClientDisconnected(
      peer: Peer,
      forceReconnect: Boolean,
      state: DataMessageHandlerState): Future[Unit] = {
    finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        logger.info(s"Client stopped for $peer peers=$peers")
        if (finder.hasPeer(peer)) {
          //client actor for one of the test peers stopped, can remove it from map now
          finder.removePeer(peer)
          Future.unit
        } else if (peerDataMap.contains(peer)) {
          //actor stopped for one of the persistent peers, can happen in case a reconnection attempt failed due to
          //reconnection tries exceeding the max limit in which the client was stopped to disconnect from it, remove it
          _peerDataMap.remove(peer)
          //getDataMesageHandler.state is already mutated from another thread
          //this will be set to the new sync peer not the old one.
          val syncPeerOpt = state match {
            case s: SyncDataMessageHandlerState =>
              Some(s.syncPeer)
            case m: MisbehavingPeer => Some(m.badPeer)
            case DoneSyncing | _: RemovePeers =>
              None
          }
          val shouldReconnect =
            (forceReconnect || connectedPeerCount == 0) && isStarted.get
          if (peers.exists(_ != peer) && syncPeerOpt.isDefined) {
            syncFromNewPeer().map(_ => ())
          } else if (syncPeerOpt.isDefined) {
            if (shouldReconnect) {
              finder.reconnect(peer)
            } else {
              val exn = new RuntimeException(
                s"No new peers to sync from, cannot start new sync. Terminated sync with peer=$peer current syncPeer=$syncPeerOpt state=${state} peers=$peers")
              Future.failed(exn)
            }
          } else {
            if (shouldReconnect) {
              finder.reconnect(peer)
            } else {
              Future.unit
            }
          }
        } else if (waitingForDeletion.contains(peer)) {
          //a peer we wanted to disconnect has remove has stopped the client actor, finally mark this as deleted
          _waitingForDeletion.remove(peer)
          Future.unit
        } else {
          logger.warn(s"onP2PClientStopped called for unknown $peer")
          Future.unit
        }
      case None =>
        logger.warn(
          s"onP2PClientStopped cannot be run, PeerFinder was not started")
        Future.unit
    }
  }

  def onVersionMessage(peer: Peer, versionMsg: VersionMessage): Unit = {
    finderOpt match {
      case Some(finder) =>
        require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                s"$peer cannot be both a test and a persistent peer")

        if (finder.hasPeer(peer)) {
          finder.getData(peer).get.setServiceIdentifier(versionMsg.services)
        } else if (peerDataMap.contains(peer)) {
          require(
            peerDataMap(
              peer).serviceIdentifier.bytes == versionMsg.services.bytes)
        } else {
          logger.warn(s"onVersionMessage called for unknown $peer")
        }
      case None =>
        logger.warn(
          s"onVersionMessage cannot be run, PeerFinder was not started")
        ()
    }

  }

  private def onQueryTimeout(
      payload: ExpectsResponse,
      peer: Peer,
      state: DataMessageHandlerState): Future[Unit] = {
    logger.debug(s"Query timeout out for $peer")

    //if we are removing this peer and an existing query timed out because of that
    // peerData will not have this peer
    if (peerDataMap.contains(peer)) {
      peerDataMap(peer).updateLastFailureTime()
    }

    payload match {
      case _: GetHeadersMessage =>
        offer(HeaderTimeoutWrapper(peer)).map(_ => ())
      case _ =>
        val syncPeer = state match {
          case syncState: SyncDataMessageHandlerState =>
            syncState.syncPeer
          case s @ (DoneSyncing | _: MisbehavingPeer | _: RemovePeers) =>
            sys.error(s"Cannot have state=$s and have a query timeout")
        }
        if (peer == syncPeer)
          syncFromNewPeer().map(_ => ())
        else Future.unit
    }
  }

  private def onHeaderRequestTimeout(
      peer: Peer,
      dmh: DataMessageHandler): Future[DataMessageHandler] = {
    val state = dmh.state
    logger.info(s"Header request timed out from $peer in state $state")
    state match {
      case HeaderSync(_) | MisbehavingPeer(_) | DoneSyncing =>
        syncFromNewPeer().map(_ => dmh)
      case headerState @ ValidatingHeaders(_, _, failedCheck, _) =>
        val newHeaderState = headerState.copy(failedCheck = failedCheck + peer)
        val newDmh = dmh.copy(state = newHeaderState)

        if (newHeaderState.validated) {
          PeerManager.fetchCompactFilterHeaders(newDmh, this)
        } else Future.successful(newDmh)

      case _: FilterHeaderSync | _: FilterSync | _: RemovePeers =>
        Future.successful(dmh)
    }
  }

  private def sendResponseTimeout(
      peer: Peer,
      payload: NetworkPayload): Future[Unit] = {
    logger.debug(
      s"Sending response timeout for ${payload.commandName} to $peer")
    if (peerDataMap.contains(peer)) {
      peerDataMap(peer).peerMessageSender.map(
        _.client.actor ! ResponseTimeout(payload))
    } else {
      logger.debug(s"Requested to send response timeout for unknown $peer")
      Future.unit
    }
  }

  private def buildStatelessDataMessagehandler(
      queue: SourceQueueWithComplete[
        StreamDataMessageWrapper]): DataMessageHandler = {
    DataMessageHandler(
      chainApi = ChainHandler.fromDatabase(),
      walletCreationTimeOpt = walletCreationTimeOpt,
      queue = queue,
      peers = Vector.empty,
      peerMessgeSenderApi = this,
      peerDataOpt = None,
      state = DoneSyncing,
      filterBatchCache = Set.empty
    )
  }

  //can this queue be reused if we PeerManager.start()/PeerManager.stop()
  //is called?
  private val dataMessageStreamSource: Source[
    StreamDataMessageWrapper,
    SourceQueueWithComplete[StreamDataMessageWrapper]] = {
    Source
      .queue[StreamDataMessageWrapper](
        16 * nodeAppConfig.maxConnectedPeers,
        overflowStrategy = OverflowStrategy.backpressure,
        maxConcurrentOffers = nodeAppConfig.maxConnectedPeers)
  }

  private def buildDataMessageStreamSink(initDmh: DataMessageHandler): Sink[
    StreamDataMessageWrapper,
    Future[DataMessageHandler]] = {
    Sink.foldAsync(initDmh) {
      case (dmh, sendToPeer: SendToPeer) =>
        logger.debug(
          s"Sending message ${sendToPeer.msg.payload.commandName} to peerOpt=${sendToPeer.peerOpt}")
        val peerMsgSenderOptF: Future[Option[PeerMessageSender]] =
          sendToPeer.peerOpt match {
            case Some(peer) =>
              getPeerMsgSender(peer).flatMap {
                case Some(peerMsgSender) =>
                  Future.successful(Some(peerMsgSender))
                case None =>
                  sendToPeer.msg.payload match {
                    case _: ControlPayload =>
                      //peer may not be fully initialized, we may be doing the handshake with a peer
                      finderOpt.get
                        .getData(peer)
                        .map(_.peerMessageSender) match {
                        case Some(p) => p.map(Some(_))
                        case None    => FutureUtil.none
                      }
                    case _: DataPayload =>
                      //peer must be fully initialized to send a data payload
                      val msg =
                        s"Cannot find peerOpt=${sendToPeer.peerOpt} to send message=${sendToPeer.msg.payload.commandName} to. It may have been disconnected, sending to another random peer."
                      logger.warn(msg)
                      randomPeerMsgSenderWithService(
                        ServiceIdentifier.NODE_NETWORK)
                  }
              }
            case None =>
              dmh.state match {
                case s: SyncDataMessageHandlerState =>
                  getPeerMsgSender(s.syncPeer)
                case DoneSyncing | _: MisbehavingPeer | _: RemovePeers =>
                  //pick a random peer to sync with
                  randomPeerMsgSenderWithService(ServiceIdentifier.NODE_NETWORK)
              }
          }

        peerMsgSenderOptF.flatMap {
          case Some(peerMsgSender) =>
            peerMsgSender
              .sendMsg(sendToPeer.msg)
              .map(_ => handleDisconnectedPeer(sendToPeer, peerMsgSender, dmh))
          case None =>
            Future.failed(new RuntimeException(
              s"Unable to find peer message sender to send msg=${sendToPeer.msg.header.commandName} to"))
        }
      case (dmh, _ @DataMessageWrapper(payload, peer)) =>
        logger.debug(s"Got ${payload.commandName} from peer=${peer} in stream")
        val peerMsgSenderOptF = getPeerMsgSender(peer)
        peerMsgSenderOptF.flatMap {
          case None =>
            Future.failed(new RuntimeException(
              s"Couldn't find PeerMessageSender that corresponds with peer=$peer msg=${payload.commandName}. Was it disconnected?"))
          case Some(_) =>
            val peerDmh = dmh.copy(peerDataOpt = getPeerData(peer))

            val resultF = peerDmh
              .handleDataPayload(payload, peer)
              .flatMap { newDmh =>
                newDmh.state match {
                  case m: MisbehavingPeer =>
                    //disconnect the misbehaving peer
                    for {
                      _ <- removePeer(m.badPeer)
                      _ <- syncFromNewPeer()
                    } yield newDmh
                  case removePeers: RemovePeers =>
                    for {
                      _ <- Future.traverse(removePeers.peers)(removePeer)
                    } yield newDmh
                  case _: SyncDataMessageHandlerState | DoneSyncing =>
                    Future.successful(newDmh)
                }
              }
            resultF.map { r =>
              logger.debug(
                s"Done processing ${payload.commandName} in peer=${peer}")
              r
            }
        }
      case (dmh, _ @HeaderTimeoutWrapper(peer)) =>
        logger.debug(s"Processing timeout header for $peer")
        for {
          newDmh <- {
            onHeaderRequestTimeout(peer, dmh).map { newDmh =>
              logger.debug(s"Done processing timeout header for $peer")
              newDmh
            }
          }
        } yield newDmh
      case (dmh, _ @DisconnectedPeer(peer, forceReconnect)) =>
        onP2PClientDisconnected(peer, forceReconnect, dmh.state).map(_ => dmh)
      case (dmh, i: Initialized) =>
        onInitialization(i.peer).map(_ => dmh)
      case (dmh, i: InitializationTimeout) =>
        onInitializationTimeout(i.peer).map(_ => dmh)
      case (dmh, q: QueryTimeout) =>
        onQueryTimeout(q.payload, q.peer, dmh.state).map(_ => dmh)
      case (dmh, srt: SendResponseTimeout) =>
        sendResponseTimeout(srt.peer, srt.payload).map(_ => dmh)
    }
  }

  private val decider: Supervision.Decider = { case err: Throwable =>
    logger.error(s"Error occurred while processing p2p pipeline stream", err)
    Supervision.Resume
  }

  private def buildDataMessageStreamGraph(
      initDmh: DataMessageHandler,
      source: Source[StreamDataMessageWrapper, NotUsed]): RunnableGraph[
    Future[DataMessageHandler]] = {
    val graph = source
      .toMat(buildDataMessageStreamSink(initDmh))(Keep.right)
      .withAttributes(ActorAttributes.supervisionStrategy(decider))
    graph
  }

  private[bitcoins] var dataMessageQueueOpt: Option[
    SourceQueueWithComplete[StreamDataMessageWrapper]] = None

  private var streamDoneFOpt: Option[Future[DataMessageHandler]] = None

  override def offer(
      elem: StreamDataMessageWrapper): Future[QueueOfferResult] = {
    dataMessageQueueOpt match {
      case Some(queue) => queue.offer(elem)
      case None =>
        Future.failed(new RuntimeException(
          s"PeerManager not started, cannot process p2p message until PeerManager.start() is called"))
    }
  }

  override def watchCompletion(): Future[Done] = {
    dataMessageQueueOpt match {
      case Some(queue) => queue.watchCompletion()
      case None        => Future.successful(Done)
    }
  }

  /** Helper method to sync the blockchain over the network
    *
    * @param syncPeerOpt if syncPeer is given, we send [[org.bitcoins.core.p2p.GetHeadersMessage]] to that peer. If None we gossip GetHeadersMessage to all peers
    */
  def syncHelper(syncPeerOpt: Option[Peer]): Future[Unit] = {
    logger.info(s"Syncing with peerOpt=$syncPeerOpt")
    val chainApi: ChainApi = ChainHandler.fromDatabase()
    val blockchainsF =
      BlockHeaderDAO()(ec, chainAppConfig).getBlockchains()
    for {
      header <- chainApi.getBestBlockHeader()
      bestFilterHeaderOpt <- chainApi.getBestFilterHeader()
      bestFilterOpt <- chainApi.getBestFilter()
      blockchains <- blockchainsF
      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE)
      _ <- {
        syncPeerOpt match {
          case Some(peer) =>
            sendGetHeadersMessage(cachedHeaders, Some(peer))
          case None => gossipGetHeadersMessage(cachedHeaders)
        }
      }
      hasStaleTip <- chainApi.isTipStale()
      _ <- {
        if (hasStaleTip) {
          //if we have a stale tip, we will request to sync filter headers / filters
          //after we are done syncing block headers
          Future.unit
        } else {
          syncFilters(bestFilterHeaderOpt = bestFilterHeaderOpt,
                      bestFilterOpt = bestFilterOpt,
                      bestBlockHeader = header,
                      chainApi = chainApi)
        }
      }
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE.hex}")
    }
  }

  private def syncFilters(
      bestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      bestFilterOpt: Option[CompactFilterDb],
      bestBlockHeader: BlockHeaderDb,
      chainApi: ChainApi): Future[Unit] = {
    // If we have started syncing filters headers
    (bestFilterHeaderOpt, bestFilterOpt) match {
      case (None, None) | (None, Some(_)) =>
        //do nothing if we haven't started syncing
        Future.unit
      case (Some(bestFilterHeader), Some(bestFilter)) =>
        val isFilterHeaderSynced =
          bestFilterHeader.blockHashBE == bestBlockHeader.hashBE
        val isFiltersSynced = {
          //check if we have started syncing filters,
          //and if so, see if filter headers and filters
          //were in sync
          bestFilter.hashBE == bestFilterHeader.filterHashBE
        }
        if (isFilterHeaderSynced && isFiltersSynced) {
          //means we are in sync, with filter heads & block headers & filters
          //if there _both_ filter headers and block headers are on
          //an old tip, our event driven node will start syncing
          //filters after block headers are in sync
          //do nothing
          Future.unit
        } else {
          syncCompactFilters(bestFilterHeader = bestFilterHeader,
                             chainApi = chainApi,
                             bestFilterOpt = Some(bestFilter))
        }
      case (Some(bestFilterHeader), None) =>
        syncCompactFilters(bestFilterHeader = bestFilterHeader,
                           chainApi = chainApi,
                           bestFilterOpt = None)
    }
  }

  def syncFromNewPeer(): Future[Option[Peer]] = {
    for {
      syncPeerOpt <- randomPeerWithService(
        ServiceIdentifier.NODE_COMPACT_FILTERS)
      _ <- syncHelper(syncPeerOpt)
    } yield syncPeerOpt
  }

  /** Handles the case where we need to send a message to a peer, but that peer was disconnected
    * We change the peer and the adjust the state in [[DataMessageHandler]]
    * @param sendToPeer the peer we were originally sending the message to
    * @param peerMessageSender the new peer we are going to send the message to
    * @param dmh the data message handler we need to adjust state of
    */
  private def handleDisconnectedPeer(
      sendToPeer: SendToPeer,
      peerMessageSender: PeerMessageSender,
      dmh: DataMessageHandler): DataMessageHandler = {
    val destination = peerMessageSender.client.peer
    val newState: DataMessageHandlerState = sendToPeer.peerOpt match {
      case Some(originalPeer) =>
        if (originalPeer != destination) {
          //need to replace syncPeer with newSyncPeer
          dmh.state match {
            case s: SyncDataMessageHandlerState =>
              s.replaceSyncPeer(destination)
            case m: MisbehavingPeer => m
            case r: RemovePeers     => r
            case DoneSyncing        => DoneSyncing
          }
        } else {
          dmh.state
        }
      case None =>
        dmh.state
    }

    dmh.copy(state = newState)
  }
}

case class ResponseTimeout(payload: NetworkPayload)

object PeerManager extends Logging {

  def sendFirstGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      peer: Peer)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[DataMessageHandlerState] = {
    for {
      bestFilterHeaderOpt <-
        chainApi
          .getBestFilterHeader()
      filterCount <- chainApi.getFilterCount()
      blockHash = bestFilterHeaderOpt match {
        case Some(filterHeaderDb) =>
          filterHeaderDb.blockHashBE
        case None =>
          DoubleSha256DigestBE.empty
      }
      hashHeightOpt <- chainApi.nextBlockHeaderBatchRange(
        prevStopHash = blockHash,
        batchSize = chainConfig.filterHeaderBatchSize)
      res <- hashHeightOpt match {
        case Some(filterSyncMarker) =>
          peerMessageSenderApi
            .sendGetCompactFilterHeadersMessage(filterSyncMarker, Some(peer))
            .map(_ => FilterHeaderSync(peer))
        case None =>
          sys.error(
            s"Could not find block header in database to sync filter headers from! It's likely your database is corrupted blockHash=$blockHash bestFilterHeaderOpt=$bestFilterHeaderOpt filterCount=$filterCount")
      }
    } yield res
  }

  def sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      peer: Peer,
      filterHeaderBatchSize: Int,
      prevStopHash: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <- chainApi.nextBlockHeaderBatchRange(
        prevStopHash = prevStopHash,
        batchSize = filterHeaderBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(
            s"Requesting next compact filter headers from $filterSyncMarker")
          peerMessageSenderApi
            .sendGetCompactFilterHeadersMessage(filterSyncMarker, Some(peer))
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  /** @return a flag indicating if we are syncing or not
    */
  def sendNextGetCompactFilterCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      filterBatchSize: Int,
      startHeight: Int,
      peer: Peer)(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <-
        chainApi.nextFilterHeaderBatchRange(startHeight, filterBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(s"Requesting compact filters from $filterSyncMarker")

          peerMessageSenderApi
            .sendGetCompactFiltersMessage(filterSyncMarker, peer)
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  def fetchCompactFilterHeaders(
      currentDmh: DataMessageHandler,
      peerMessageSenderApi: PeerMessageSenderApi)(implicit
      ec: ExecutionContext,
      nodeAppConfig: NodeAppConfig,
      chainAppConfig: ChainAppConfig): Future[DataMessageHandler] = {
    val syncPeer = currentDmh.state match {
      case s: SyncDataMessageHandlerState => s.syncPeer
      case state @ (DoneSyncing | _: MisbehavingPeer | _: RemovePeers) =>
        sys.error(
          s"Cannot fetch compact filter headers when we are in state=$state")
    }
    logger.info(
      s"Now syncing filter headers from $syncPeer in state=${currentDmh.state}")
    for {
      newSyncingState <- PeerManager.sendFirstGetCompactFilterHeadersCommand(
        peerMessageSenderApi = peerMessageSenderApi,
        chainApi = currentDmh.chainApi,
        peer = syncPeer)
    } yield {
      currentDmh.copy(state = newSyncingState)
    }
  }
}
