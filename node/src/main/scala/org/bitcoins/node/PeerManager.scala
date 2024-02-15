package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable}
import akka.stream.scaladsl.{Sink, SourceQueue}
import grizzled.slf4j.Logging
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.api.node._
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.NodeState._
import org.bitcoins.node.NodeStreamMessage._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{PeerDAO, PeerDAOHelper, PeerDb}
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.util.PeerMessageSenderApi

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

case class PeerManager(
    paramPeers: Vector[Peer],
    walletCreationTimeOpt: Option[Instant],
    queue: SourceQueue[NodeStreamMessage])(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends StartStopAsync[PeerManager]
    with PeerManagerApi
    with P2PLogger {
  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  private val _peerDataMap: mutable.Map[Peer, PersistentPeerData] =
    mutable.Map.empty
  def connectedPeerCount: Int = _peerDataMap.size

  override def connectPeer(peer: Peer): Future[Unit] = {
    val c = ConnectPeer(peer)
    queue.offer(c).map(_ => ())
  }

  override def peers: Set[Peer] = _peerDataMap.keys.toSet

  def peersWithServices: Set[PeerWithServices] = {
    peerDataMap.map(_._2.peerWithServicesOpt).flatten.toSet
  }

  def peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData] = {
    peerDataMap.map(t => (t._2.peerWithServicesOpt.get, t._2))
  }

  /** Starts sync compact filter headers.
    * Only starts syncing compact filters if our compact filter headers are in sync with block headers
    */
  private def syncCompactFilters(
      bestFilterHeader: CompactFilterHeaderDb,
      chainApi: ChainApi,
      compactFilterStartHeightOpt: Option[Int],
      nodeState: SyncNodeState)(implicit
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val syncPeer = nodeState.syncPeer
    val peerMsgSender = nodeState.getPeerMsgSender(syncPeer) match {
      case Some(p) => p
      case None =>
        sys.error(s"Could not find peer=$syncPeer")
    }
    val bestBlockHashF = chainApi.getBestBlockHash()
    val sendCompactFilterHeaderMsgF = bestBlockHashF.flatMap { bestBlockHash =>
      PeerManager.sendNextGetCompactFilterHeadersCommand(
        peerMessageSenderApi = peerMsgSender,
        chainApi = chainApi,
        peer = syncPeer,
        filterHeaderBatchSize = chainAppConfig.filterHeaderBatchSize,
        prevStopHash = bestFilterHeader.blockHashBE,
        stopHash = bestBlockHash
      )
    }

    sendCompactFilterHeaderMsgF.flatMap { isSyncFilterHeaders =>
      // If we have started syncing filters
      if (!isSyncFilterHeaders) {
        PeerManager
          .sendNextGetCompactFilterCommand(
            peerMessageSenderApi = peerMsgSender,
            chainApi = chainApi,
            filterBatchSize = chainAppConfig.filterBatchSize,
            startHeightOpt = compactFilterStartHeightOpt,
            stopBlockHash = bestFilterHeader.blockHashBE,
            peer = syncPeer
          )
          .map(_ => ())
      } else {
        Future.unit
      }
    }
  }

  private def createInDb(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Future[PeerDb] = {
    logger.debug(s"Adding peer to db $peer")
    val addrBytes = PeerDAOHelper.getAddrBytes(peer)
    val networkByte = addrBytes.length match {
      case AddrV2Message.IPV4_ADDR_LENGTH   => AddrV2Message.IPV4_NETWORK_BYTE
      case AddrV2Message.IPV6_ADDR_LENGTH   => AddrV2Message.IPV6_NETWORK_BYTE
      case AddrV2Message.TOR_V3_ADDR_LENGTH => AddrV2Message.TOR_V3_NETWORK_BYTE
      case unknownSize =>
        throw new IllegalArgumentException(
          s"Unsupported address type of size $unknownSize bytes")
    }
    PeerDAO()
      .upsertPeer(addrBytes, peer.port, networkByte, serviceIdentifier)
  }

  private def replacePeer(replacePeer: Peer, withPeer: Peer): Future[Unit] = {
    logger.debug(s"Replacing $replacePeer with $withPeer")
    require(!peerDataMap(replacePeer).serviceIdentifier.nodeCompactFilters,
            s"$replacePeer has cf")
    for {
      _ <- disconnectPeer(replacePeer)
      _ <- connectPeer(withPeer)
    } yield {
      ()
    }
  }

  def disconnectPeer(peer: Peer): Future[Unit] = {
    logger.debug(s"Disconnecting persistent peer=$peer")
    queue.offer(InitializeDisconnect(peer)).map(_ => ())
  }

  override def start(): Future[PeerManager] = {
    logger.debug(s"Starting PeerManager")
    isStarted.set(true)
    Future.successful(this)
  }

  private def peerDataMap: Map[Peer, PersistentPeerData] = _peerDataMap.toMap

  def getPeerData(peer: Peer): Option[PersistentPeerData] =
    peerDataMap.get(peer)

  override def stop(): Future[PeerManager] = {
    logger.info(s"Stopping PeerManager")
    isStarted.set(false)
    val beganAt = System.currentTimeMillis()

    syncFilterCancellableOpt.map(_._2.cancel())

    val stopF = for {
      _ <- queue.offer(NodeShutdown)
      _ <- AsyncUtil.retryUntilSatisfied(
        _peerDataMap.isEmpty,
        interval = 1.seconds,
        maxTries = 30
      )
      _ = {
        //reset all variables
        syncFilterCancellableOpt = None
      }
    } yield {
      logger.info(
        s"Stopped PeerManager. Took ${System.currentTimeMillis() - beganAt}ms")
      this
    }

    stopF
  }

  override def isConnected(peer: Peer): Future[Boolean] = {
    peerDataMap.get(peer) match {
      case None    => Future.successful(false)
      case Some(p) => p.peerConnection.isConnected()
    }
  }

  override def isDisconnected(peer: Peer): Future[Boolean] = {
    isConnected(peer).map(b => !b)
  }

  override def isInitialized(peer: Peer): Future[Boolean] = {
    Future.successful(peerDataMap.exists(_._1 == peer))
  }

  private def onInitializationTimeout(
      peer: Peer,
      state: NodeRunningState): Future[Unit] = {
    val finder = state.peerFinder
    require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
            s"$peer cannot be both a test and a persistent peer")

    if (finder.hasPeer(peer)) {
      //one of the peers that we tried, failed to init within time, disconnect
      finder.getPeerData(peer).get.stop().map(_ => ())
    } else if (peerDataMap.contains(peer)) {
      //this is one of our persistent peers which must have been initialized earlier, this can happen in case of
      //a reconnection attempt, meaning it got connected but failed to initialize, disconnect
      peerDataMap(peer)
        .stop()
        .map(_ => ())
    } else {
      //this should never happen
      logger.warn(s"onInitializationTimeout called for unknown $peer")
      Future.unit
    }
  }

  /** Helper method to determine what action to take after a peer is initialized, such as beginning sync with that peer */
  private def managePeerAfterInitialization(
      state: NodeRunningState,
      peer: Peer): Future[NodeRunningState] = {
    val curPeerDataOpt = state.peerFinder.getPeerData(peer)
    require(curPeerDataOpt.isDefined,
            s"Could not find peer=$peer in PeerFinder!")
    val peerData = curPeerDataOpt.get
    val hasCf = peerData.serviceIdentifier.nodeCompactFilters
    val notCfPeers = peerDataMap
      .filter(p => !p._2.serviceIdentifier.nodeCompactFilters)
      .keys
    val availableFilterSlot = hasCf && notCfPeers.nonEmpty
    val hasConnectionSlot = connectedPeerCount < nodeAppConfig.maxConnectedPeers
    if (hasConnectionSlot || availableFilterSlot) {
      //we want to promote this peer, so pop from cache
      val _ = state.peerFinder.popFromCache(peer)
      val persistentPeerData = peerData match {
        case p: PersistentPeerData       => p
        case a: AttemptToConnectPeerData => a.toPersistentPeerData
      }
      _peerDataMap.put(peer, persistentPeerData)

      val peerWithSvcs = persistentPeerData.peerWithServicesOpt.get
      val newPdm =
        state.peerDataMap.+((peerWithSvcs, persistentPeerData))
      val newState = state.replacePeers(newPdm)
      if (availableFilterSlot) {
        replacePeer(replacePeer = notCfPeers.head, withPeer = peer)
          .map(_ => newState)
      } else {
        connectPeer(peer).map(_ => newState)
      }
    } else {
      Future.successful(state)
    }
  }

  private def onInitialization(
      peer: Peer,
      state: NodeRunningState): Future[NodeState] = {
    val finder = state.peerFinder

    val stateF: Future[NodeRunningState] = {

      //this assumes neutrino and checks for compact filter support so should not be called for anything else
      require(nodeAppConfig.nodeType == NodeType.NeutrinoNode,
              s"Node cannot be ${nodeAppConfig.nodeType.shortName}")

      if (finder.hasPeer(peer)) {
        //one of the peers we tries got initialized successfully
        val peerData = finder.getPeerData(peer).get
        val serviceIdentifer = peerData.serviceIdentifier
        val hasCf = serviceIdentifer.nodeCompactFilters

        for {
          _ <- peerData.peerMessageSender.sendGetAddrMessage()
          _ <- createInDb(peer, peerData.serviceIdentifier)
          newState <- managePeerAfterInitialization(state, peer)
        } yield {
          require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
                  s"$peer cannot be both a test and a persistent peer")
          logger.debug(
            s"Initialized peer $peer with compactFilter support=$hasCf")
          newState
        }

      } else if (peerDataMap.contains(peer)) {
        //one of the persistent peers initialized again, this can happen in case of a reconnection attempt
        //which succeeded which is all good, do nothing
        state match {
          case s: SyncNodeState =>
            val x = s.replaceSyncPeer(peer)
            syncHelper(x).map(_ => x)
          case d: DoneSyncing =>
            val h = d.toHeaderSync(peer)
            syncHelper(h).map(_ => h)
          case x @ (_: RemovePeers | _: MisbehavingPeer |
              _: NodeShuttingDown) =>
            Future.successful(x)
        }
      } else {
        logger.warn(s"onInitialization called for unknown $peer")
        Future.successful(state)
      }
    }

    stateF.map { s =>
      s.replacePeers(peerWithServicesDataMap)
    }
  }

  /** @param peer the peer we were disconencted from
    * @param reconnect flag indicating if we should attempt to reconnect
    * @return
    */
  private def onDisconnect(
      peer: Peer,
      forceReconnect: Boolean,
      state: NodeRunningState): Future[NodeState] = {
    logger.info(
      s"Disconnected peer=$peer peers=$peers state=$state forceReconnect=$forceReconnect peerDataMap=${peerDataMap
        .map(_._1)}")
    val finder = state.peerFinder
    val _ = onDisconnectSyncFiltersJob(peer)
    val updateLastSeenF = PeerDAO().updateLastSeenTime(peer)
    val stateF: Future[NodeState] = {
      require(!finder.hasPeer(peer) || !peerDataMap.contains(peer),
              s"$peer cannot be both a test and a persistent peer")

      if (finder.hasPeer(peer)) {
        finder.removePeer(peer)
        Future.successful(state)
      } else if (peerDataMap.contains(peer)) {
        _peerDataMap.remove(peer)
        val isShuttingDown = state.isInstanceOf[NodeShuttingDown]
        if (state.peers.exists(_ != peer)) {
          state match {
            case s: SyncNodeState => switchSyncToRandomPeer(s, Some(peer))
            case d: DoneSyncing   =>
              //defensively try to sync with the new peer
              val hs = d.toHeaderSync
              syncHelper(hs).map(_ => hs)
            case x @ (_: DoneSyncing | _: NodeShuttingDown |
                _: MisbehavingPeer | _: RemovePeers) =>
              Future.successful(x)
          }

        } else {
          if (forceReconnect && !isShuttingDown) {
            finder.reconnect(peer).map(_ => state)
          } else if (!isShuttingDown) {
            logger.info(
              s"No new peers to connect to, querying for new connections... state=${state} peers=$peers")
            finder.queryForPeerConnections(Set(peer)) match {
              case Some(_) => Future.successful(state)
              case None =>
                logger.debug(
                  s"Could not query for more peer connections as previous job is still running")
                Future.successful(state)
            }
          } else {
            //if shutting down, do nothing
            Future.successful(state)
          }
        }
      } else if (state.waitingForDisconnection.contains(peer)) {
        //a peer we wanted to disconnect has remove has stopped the client actor, finally mark this as deleted
        val removed = state.waitingForDisconnection.removedAll(Set(peer))
        val newState = state.replaceWaitingForDisconnection(removed)
        newState match {
          case s: SyncNodeState =>
            switchSyncToRandomPeer(s, Some(peer))
          case x @ (_: DoneSyncing | _: NodeShuttingDown | _: MisbehavingPeer |
              _: RemovePeers) =>
            Future.successful(x)
        }
      } else {
        logger.warn(s"onP2PClientStopped called for unknown $peer")
        Future.successful(state)
      }
    }

    val replacedPeersStateF = stateF.map {
      case s: SyncNodeState =>
        if (s.syncPeer == peer) {
          //the peer being disconnected is our sync peer
          s.randomPeer(excludePeers = Set(peer),
                       ServiceIdentifier.NODE_COMPACT_FILTERS) match {
            case Some(p) => s.replaceSyncPeer(p)
            case None    =>
              //switch to state DoneSyncing since we have no peers to sync from
              DoneSyncing(peerDataMap = peerWithServicesDataMap,
                          waitingForDisconnection =
                            state.waitingForDisconnection,
                          peerFinder = s.peerFinder)
          }
        } else {
          s.replacePeers(peerWithServicesDataMap)
        }
      case runningState: NodeRunningState =>
        runningState.replacePeers(peerWithServicesDataMap)
    }

    for {
      state <- replacedPeersStateF
      _ <- updateLastSeenF
    } yield state
  }

  private def onQueryTimeout(
      payload: ExpectsResponse,
      peer: Peer,
      state: NodeState): Future[Unit] = {
    logger.debug(s"Query timeout out for $peer with payload=${payload}")

    //if we are removing this peer and an existing query timed out because of that
    // peerData will not have this peer
    if (peerDataMap.contains(peer)) {
      peerDataMap(peer).updateLastFailureTime()
    }

    payload match {
      case _: GetHeadersMessage =>
        queue.offer(HeaderTimeoutWrapper(peer)).map(_ => ())
      case _ =>
        state match {
          case syncState: SyncNodeState =>
            syncFromNewPeer(syncState)
              .map(_ => ())
          case s @ (_: DoneSyncing | _: MisbehavingPeer | _: RemovePeers |
              _: NodeShuttingDown) =>
            sys.error(s"Cannot have state=$s and have a query timeout")
        }

    }
  }

  /** @param peer
    * @param state
    * @return a NodeState that contains the new peer we are syncing with, None if we couldn't find a new peer to sync with
    */
  private def onHeaderRequestTimeout(
      peer: Peer,
      state: NodeState): Future[Option[NodeState]] = {
    logger.info(s"Header request timed out from $peer in state $state")
    state match {
      case h: HeaderSync =>
        syncFromNewPeer(h)
      case d: DoneSyncing =>
        syncFromNewPeer(d)
      case x: MisbehavingPeer =>
        syncFromNewPeer(x)

      case _: FilterHeaderSync | _: FilterSync | _: RemovePeers |
          _: NodeShuttingDown =>
        Future.successful(Some(state))
    }
  }

  private def sendResponseTimeout(
      peer: Peer,
      payload: NetworkPayload): Future[Unit] = {
    logger.debug(
      s"Sending response timeout for ${payload.commandName} to $peer")
    if (peerDataMap.contains(peer)) {
      payload match {
        case e: ExpectsResponse =>
          queue
            .offer(QueryTimeout(peer, e))
            .map(_ => ())
        case _: NetworkPayload =>
          val exn = new RuntimeException(
            s"Cannot have sendResponseTimeout for msg=${payload.commandName} for non ExpectsResponse payload")
          Future.failed(exn)
      }
    } else {
      logger.debug(s"Requested to send response timeout for unknown $peer")
      Future.unit
    }
  }

  def buildP2PMessageHandlerSink(
      initState: NodeState): Sink[NodeStreamMessage, Future[NodeState]] = {
    Sink.foldAsync(initState) {
      case (state, s: StartSync) =>
        val nodeStateOptF: Future[Option[NodeState]] = s.peerOpt match {
          case Some(p) =>
            state match {
              case s: SyncNodeState if !s.waitingForDisconnection.contains(p) =>
                switchSyncToPeer(s, p).map(Some(_))
              case s: SyncNodeState =>
                logger.warn(
                  s"Ignoring sync request for peer=${p} as its waiting for disconnection")
                Future.successful(Some(s))
              case x @ (_: MisbehavingPeer | _: RemovePeers) =>
                logger.warn(
                  s"Ignoring sync request for peer=${p} while we are in state=$x")
                Future.successful(Some(x)) //ignore sync request?
              case s: NodeShuttingDown =>
                logger.warn(
                  s"Ignoring sync request as our node is shutting down, state=$s")
                Future.successful(Some(s))
              case d: DoneSyncing =>
                val h =
                  HeaderSync(p,
                             d.peerDataMap,
                             d.waitingForDisconnection,
                             d.peerFinder)
                syncFromNewPeer(h)
            }
          case None =>
            state match {
              case x @ (_: SyncNodeState | _: MisbehavingPeer | _: RemovePeers |
                  _: NodeShuttingDown) =>
                //we are either syncing already, or we are in a bad state to start a sync
                Future.successful(Some(x))
              case d: DoneSyncing =>
                d.randomPeer(Set.empty,
                             ServiceIdentifier.NODE_COMPACT_FILTERS) match {
                  case Some(p) =>
                    val h =
                      HeaderSync(p,
                                 d.peerDataMap,
                                 d.waitingForDisconnection,
                                 d.peerFinder)
                    syncFromNewPeer(h)
                  case None =>
                    Future.successful(None)
                }
            }
        }
        nodeStateOptF.map {
          case Some(ns) => ns
          case None =>
            logger.warn(
              s"Cannot find a new peer to fulfill sync request, reverting to old state=$state")
            state
        }
      case (state, c: ConnectPeer) =>
        state match {
          case s: NodeShuttingDown =>
            logger.warn(
              s"Ignoring connect peer request as node is shutting down, c=$c")
            Future.successful(s)
          case runningState: NodeRunningState =>
            val peer = c.peer
            val isConnectedAlready = runningState.isConnected(peer)
            if (!isConnectedAlready) {
              val connectF = runningState.peerFinder.connect(c.peer)
              connectF.map(_ => runningState)
            } else {
              val hasCf = runningState.peerDataMap
                .filter(_._1.peer == peer)
                .headOption match {
                case Some(p) => p._1.services.nodeCompactFilters
                case None    => false
              }
              logger.info(
                s"Connected to peer $peer with compact filter support=$hasCf. Connected peer count ${runningState.peerDataMap.size}")
              state match {
                case s: SyncNodeState =>
                  syncHelper(s).map(_ => s)
                case d: DoneSyncing =>
                  val x = d.toHeaderSync(c.peer)
                  syncHelper(x).map(_ => x)
                case x @ (_: MisbehavingPeer | _: RemovePeers |
                    _: NodeShuttingDown) =>
                  Future.successful(x)
              }
            }
        }
      case (state, i: InitializeDisconnect) =>
        state match {
          case r: NodeRunningState =>
            val client: PeerData =
              r.peerDataMap.find(_._1.peer == i.peer) match {
                case Some((_, p)) => p
                case None =>
                  sys.error(
                    s"Cannot find peer=${i.peer} for InitializeDisconnect=$i")
              }
            //so we need to remove if from the map for connected peers so no more request could be sent to it but we before
            //the actor is stopped we don't delete it to ensure that no such case where peers is deleted but actor not stopped
            //leading to a memory leak may happen

            //now send request to stop actor which will be completed some time in future
            client.stop().map { _ =>
              val newWaiting = r.waitingForDisconnection.+(i.peer)
              val newPdm = r.peerDataMap.filterNot(_._1.peer == i.peer)
              val newState = r
                .replaceWaitingForDisconnection(newWaiting)
                .replacePeers(newPdm)
              newState
            }
        }

      case (state, DataMessageWrapper(payload, peer)) =>
        logger.debug(s"Got ${payload.commandName} from peer=${peer} in stream")
        state match {
          case runningState: NodeRunningState =>
            val peerDataOpt = runningState.peerDataMap
              .find(_._1.peer == peer)
              .map(_._2)
            peerDataOpt match {
              case None =>
                logger.warn(
                  s"Ignoring received msg=${payload.commandName} from peer=$peer because it was disconnected, peers=$peers state=${state}")
                Future.successful(state)
              case Some(peerData) =>
                val peerMsgSender = PeerMessageSender(peerData.peerConnection)
                val dmh = DataMessageHandler(
                  chainApi = ChainHandler.fromDatabase(),
                  walletCreationTimeOpt = walletCreationTimeOpt,
                  peerMessageSenderApi = peerMsgSender,
                  peerManager = this,
                  state = runningState
                )
                val resultF: Future[NodeState] = dmh
                  .handleDataPayload(payload, peerData)
                  .flatMap { newDmh =>
                    newDmh.state match {
                      case m: MisbehavingPeer =>
                        //disconnect the misbehaving peer
                        for {
                          _ <- disconnectPeer(m.badPeer)
                        } yield {
                          runningState
                        }
                      case removePeers: RemovePeers =>
                        for {
                          _ <- Future.traverse(removePeers.peers)(
                            disconnectPeer)
                        } yield newDmh.state
                      case x @ (_: SyncNodeState | _: DoneSyncing |
                          _: NodeShuttingDown) =>
                        Future.successful(x)
                    }
                  }
                resultF.map { r =>
                  logger.debug(
                    s"Done processing ${payload.commandName} in peer=${peer} state=${r}")
                  r
                }
            }
        }

      case (state, ControlMessageWrapper(payload, peer)) =>
        state match {
          case runningState: NodeRunningState =>
            val peerMsgSenderApiOpt: Option[PeerMessageSenderApi] = {
              runningState.getPeerMsgSender(peer) match {
                case Some(p) => Some(p)
                case None =>
                  runningState.peerFinder.getPeerData(peer) match {
                    case Some(p) => Some(p.peerMessageSender)
                    case None    => None
                  }
              }
            }
            peerMsgSenderApiOpt match {
              case Some(peerMsgSenderApi) =>
                val resultOptF = runningState.peerFinder.controlMessageHandler
                  .handleControlPayload(payload,
                                        peerMsgSenderApi = peerMsgSenderApi)
                resultOptF.flatMap {
                  case Some(i: ControlMessageHandler.Initialized) =>
                    onInitialization(i.peer, runningState)
                  case Some(ControlMessageHandler.ReceivedAddrMessage) =>
                    if (runningState.peerFinder.hasPeer(peer)) {
                      //got to disconnect it since it hasn't been promoted to a persistent peer
                      runningState.peerFinder.getPeerData(peer) match {
                        case Some(pd: AttemptToConnectPeerData) =>
                          pd.stop().map(_ => runningState)
                        case None | Some(_: PersistentPeerData) =>
                          Future.successful(runningState)
                      }
                    } else {
                      //do nothing as its a persistent peer
                      Future.successful(runningState)
                    }
                  case None =>
                    Future.successful(state)
                }
              case None =>
                logger.warn(
                  s"Cannot find a peer message sender api from peer=$peer to handle control payload=${payload.commandName}")
                Future.successful(state)
            }
        }

      case (state, HeaderTimeoutWrapper(peer)) =>
        logger.debug(s"Processing timeout header for $peer")
        state match {
          case runningState: NodeRunningState =>
            for {
              newState <- {
                onHeaderRequestTimeout(peer, state).map {
                  case Some(s) => s
                  case None    =>
                    //we don't have a state to represent no connected peers atm, so switch to DoneSyncing?
                    DoneSyncing(peerDataMap = Map.empty,
                                runningState.waitingForDisconnection,
                                runningState.peerFinder)
                }
              }
            } yield {
              logger.debug(s"Done processing timeout header for $peer")
              newState
            }
        }

      case (state, DisconnectedPeer(peer, forceReconnect)) =>
        state match {
          case runningState: NodeRunningState =>
            onDisconnect(peer, forceReconnect, runningState)
        }

      case (state, i: InitializationTimeout) =>
        state match {
          case r: NodeRunningState =>
            onInitializationTimeout(i.peer, r).map(_ => r)
        }

      case (state, q: QueryTimeout) =>
        onQueryTimeout(q.payload, q.peer, state).map(_ => state)
      case (state, srt: SendResponseTimeout) =>
        sendResponseTimeout(srt.peer, srt.payload).map(_ => state)
      case (state, gossipMessage: GossipMessage) =>
        state match {
          case runningState: NodeRunningState =>
            val msg = gossipMessage.msg.payload
            val gossipPeers = gossipMessage.excludePeerOpt match {
              case Some(excludedPeer) =>
                runningState.peers
                  .filterNot(_ == excludedPeer)
              case None => runningState.peers
            }
            if (gossipPeers.isEmpty) {
              logger.warn(
                s"We have 0 peers to gossip message=${msg.commandName} to state=$state.")
              Future.successful(state)
            } else {
              Future
                .traverse(gossipPeers) { p =>
                  runningState.getPeerConnection(p) match {
                    case Some(pc) =>
                      val sender = PeerMessageSender(pc)
                      sender.sendMsg(msg)
                    case None =>
                      logger.warn(
                        s"Attempting to gossip to peer that is available in state.peers, but not peerDataMap? state=$state peerDataMap=${peerDataMap
                          .map(_._1)}")
                      Future.unit
                  }
                }
                .map(_ => state)
            }
        }
      case (state, stp: SendToPeer) =>
        state match {
          case _: NodeShuttingDown =>
            logger.warn(
              s"Cannot send to peer when we are shutting down! stp=$stp state=$state")
            Future.successful(state)
          case r: NodeRunningState =>
            sendToPeerHelper(r, stp)
        }

      case (state, NodeShutdown) =>
        state match {
          case s: NodeShuttingDown =>
            logger.warn(
              s"Shut down already requested, ignoring new shutdown request")
            Future.successful(s)
          case r: NodeRunningState =>
            val shutdownState =
              NodeShuttingDown(peerDataMap = r.peerDataMap,
                               waitingForDisconnection =
                                 r.waitingForDisconnection,
                               peerFinder = r.peerFinder)
            Future
              .traverse(r.peers)(disconnectPeer(_))
              .map(_ => shutdownState)

        }

      case (state, NodeStreamMessage.PeerHealthCheck) =>
        state match {
          case s: NodeShuttingDown =>
            logger.trace(s"Ignorinng peer health check as we are shutting down")
            Future.successful(s)
          case r: NodeRunningState =>
            PeerManager.handleHealthCheck(r)
        }

    }
  }

  private def switchSyncToRandomPeer(
      state: SyncNodeState,
      excludePeerOpt: Option[Peer]): Future[NodeState] = {
    val randomPeerOpt =
      state.randomPeer(excludePeers = excludePeerOpt.toSet,
                       ServiceIdentifier.NODE_COMPACT_FILTERS)
    randomPeerOpt match {
      case Some(peer) =>
        switchSyncToPeer(oldSyncState = state, newPeer = peer)
      case None =>
        //if we have no new peers should we just switch to DoneSyncing?
        Future.successful(state)
    }
  }

  private def sendToPeerHelper(
      state: NodeRunningState,
      stp: SendToPeer): Future[NodeRunningState] = {
    val peerMsgSenderOpt = stp.peerOpt match {
      case Some(p) =>
        state.getPeerMsgSender(p)
      case None =>
        state.randomPeerMessageSender(Set.empty,
                                      ServiceIdentifier.NODE_COMPACT_FILTERS)
    }

    peerMsgSenderOpt match {
      case Some(pms) =>
        pms
          .sendMsg(stp.msg.payload)
          .map(_ => state)
      case None =>
        logger.warn(
          s"Unable to find peer to send message=${stp.msg.payload} to, state=$state")
        Future.successful(state)
    }
  }

  private def switchSyncToPeer(
      oldSyncState: SyncNodeState,
      newPeer: Peer): Future[NodeState] = {
    logger.debug(
      s"switchSyncToPeer() oldSyncState=$oldSyncState newPeer=$newPeer")
    val newState = oldSyncState.replaceSyncPeer(newPeer)
    oldSyncState match {
      case s: HeaderSync =>
        if (s.syncPeer != newPeer) {
          syncHelper(newState).map(_ => newState)
        } else {
          //if its same peer we don't need to switch
          Future.successful(oldSyncState)
        }
      case s @ (_: FilterHeaderSync | _: FilterSync) =>
        if (s.syncPeer != newPeer) {
          filterSyncHelper(chainApi = ChainHandler.fromDatabase(),
                           syncNodeState = newState).map(_ => newState)
        } else {
          //if its same peer we don't need to switch
          Future.successful(oldSyncState)
        }

    }
  }

  /** If [[syncPeerOpt]] is given, we send getheaders to only that peer, if no sync peer given we gossip getheaders to all our peers */
  private def getHeaderSyncHelper(
      syncNodeState: SyncNodeState): Future[Unit] = {
    val blockchainsF =
      BlockHeaderDAO()(ec, chainAppConfig).getBlockchains()

    for {
      blockchains <- blockchainsF
      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE)
      _ <- {
        syncNodeState.getPeerMsgSender(syncNodeState.syncPeer) match {
          case Some(peerMsgSender) =>
            peerMsgSender.sendGetHeadersMessage(cachedHeaders)
          case None =>
            gossipGetHeadersMessage(cachedHeaders)
        }
      }
    } yield ()
  }

  private def filterSyncHelper(
      chainApi: ChainApi,
      syncNodeState: SyncNodeState): Future[Unit] = {
    for {
      header <- chainApi.getBestBlockHeader()
      bestFilterHeaderOpt <- chainApi.getBestFilterHeader()
      bestFilterOpt <- chainApi.getBestFilter()

      hasStaleTip <- chainApi.isTipStale()
      _ <- {
        if (hasStaleTip) {
          //if we have a stale tip, we will request to sync filter headers / filters
          //after we are done syncing block headers
          Future.unit
        } else {
          val fhs = FilterHeaderSync(syncPeer = syncNodeState.syncPeer,
                                     peerDataMap = peerWithServicesDataMap,
                                     waitingForDisconnection = Set.empty,
                                     syncNodeState.peerFinder)
          syncFilters(
            bestFilterHeaderOpt = bestFilterHeaderOpt,
            bestFilterOpt = bestFilterOpt,
            bestBlockHeader = header,
            chainApi = chainApi,
            nodeState = fhs
          )
        }
      }
    } yield ()
  }

  /** Scheduled job to sync compact filters */
  @volatile private[this] var syncFilterCancellableOpt: Option[
    (Peer, Cancellable)] =
    None

  def sync(syncPeerOpt: Option[Peer]): Future[Unit] = {
    val s = StartSync(syncPeerOpt)
    queue.offer(s).map(_ => ())
  }

  /** Helper method to sync the blockchain over the network
    *
    * @param syncPeerOpt if syncPeer is given, we send [[org.bitcoins.core.p2p.GetHeadersMessage]] to that peer. If None we gossip GetHeadersMessage to all peers
    */
  private def syncHelper(syncNodeState: SyncNodeState): Future[Unit] = {
    val syncPeer = syncNodeState.syncPeer
    logger.debug(
      s"syncHelper() syncPeer=$syncPeer isStarted.get=${isStarted.get} syncFilterCancellableOpt.isDefined=${syncFilterCancellableOpt.isDefined}")
    val chainApi: ChainApi = ChainHandler.fromDatabase()
    val headerF = chainApi.getBestBlockHeader()
    val filterHeaderCountF = chainApi.getFilterHeaderCount()
    val filterCountF = chainApi.getFilterCount()
    for {
      _ <- chainApi.setSyncing(true)
      _ <- getHeaderSyncHelper(syncNodeState)
      _ = {
        if (isStarted.get) {
          //in certain cases, we can schedule this job while the peer manager is attempting to shutdown
          //this is because we start syncing _after_ the connection to the peer is established
          //while we are waiting for this connection to be established, we could decide to shutdown the PeerManager
          //if we are unlucky there could be a race condition here between
          //
          // 1. Starting to sync blockchain data from our peer we just established a connection with
          // 2. Shutting down the peer manager.
          //
          // the filter sync job gets scheduled _after_ PeerManager.stop() has been called
          syncFilterCancellableOpt = syncFilterCancellableOpt match {
            case s: Some[(Peer, Cancellable)] =>
              s //do nothing as we already have a job scheduled
            case None =>
              val c = createFilterSyncJob(chainApi, syncNodeState)
              Some(c)
          }
        }
      }
      header <- headerF
      filterHeaderCount <- filterHeaderCountF
      filterCount <- filterCountF
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE.hex} filterHeaderCount=$filterHeaderCount filterCount=$filterCount syncPeer=$syncPeer")
    }
  }

  /** If we are disconnecting a peer, we want to cancel the associated
    * sync filters job if the peer we are disconnecting is the one
    * we are scheduled to sync filters with later
    */
  private def onDisconnectSyncFiltersJob(peer: Peer): Unit = {
    syncFilterCancellableOpt match {
      case Some((p, cancellable)) =>
        if (peer == p) {
          cancellable.cancel()
          syncFilterCancellableOpt = None
          ()
        } else {
          ()
        }
      case None => ()
    }
  }

  private def createFilterSyncJob(
      chainApi: ChainApi,
      syncNodeState: SyncNodeState): (Peer, Cancellable) = {
    require(
      syncFilterCancellableOpt.isEmpty,
      s"Cannot schedule a syncFilterCancellable as one is already scheduled")
    //add a delay when syncing filter headers/filters for the case when we restart the node,
    //our block header tip _is not_ synced with the network, but our tip is also _not_ stale
    //this can result in duplicate syncing of filter headers.
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/5125
    val oldFilterHeaderCountF = chainApi.getFilterHeaderCount()
    val oldFilterCountF = chainApi.getFilterCount()
    val cancellable = system.scheduler.scheduleOnce(10.seconds) {
      val filterSyncF = {
        for {
          oldFilterHeaderCount <- oldFilterHeaderCountF
          oldFilterCount <- oldFilterCountF
          blockCount <- chainApi.getBlockCount()
          currentFilterHeaderCount <- chainApi.getFilterHeaderCount()
          currentFilterCount <- chainApi.getFilterCount()
          _ <- {
            //make sure filter sync hasn't started since we schedule the job...
            //see: https://github.com/bitcoin-s/bitcoin-s/issues/5167
            val isOutOfSync = PeerManager.isFiltersOutOfSync(
              blockCount = blockCount,
              oldFilterHeaderCount = oldFilterHeaderCount,
              currentFilterHeaderCount = currentFilterHeaderCount,
              oldFilterCount = oldFilterCount,
              currentFilterCount = currentFilterCount
            )

            if (isOutOfSync) {
              //if it hasn't started it, start it
              filterSyncHelper(chainApi, syncNodeState)
            } else {
              Future.unit
            }
          }
        } yield ()
      }
      filterSyncF.onComplete {
        case scala.util.Success(_) =>
          syncFilterCancellableOpt = None
        case scala.util.Failure(err) =>
          logger.error(s"Failed to start syncing filters", err)
          syncFilterCancellableOpt = None
      }
      ()
    }

    (syncNodeState.syncPeer, cancellable)
  }

  /** Returns true if filter are in sync with their old counts, but out of sync with our block count */

  private def syncFilters(
      bestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      bestFilterOpt: Option[CompactFilterDb],
      bestBlockHeader: BlockHeaderDb,
      chainApi: ChainApi,
      nodeState: SyncNodeState): Future[Unit] = {
    val isTipStaleF = chainApi.isTipStale()
    isTipStaleF.flatMap { isTipStale =>
      if (isTipStale) {
        logger.error(
          s"Cannot start syncing filters while blockchain tip is stale")
        Future.unit
      } else {
        logger.debug(
          s"syncFilters() bestBlockHeader=$bestBlockHeader bestFilterHeaderOpt=$bestFilterHeaderOpt bestFilterOpt=$bestFilterOpt state=$nodeState")
        // If we have started syncing filters headers
        (bestFilterHeaderOpt, bestFilterOpt) match {
          case (None, None) | (None, Some(_)) =>
            nodeState match {
              case fhs: FilterHeaderSync =>
                val peerMsgSender =
                  nodeState.getPeerMsgSender(fhs.syncPeer).get
                PeerManager
                  .sendFirstGetCompactFilterHeadersCommand(
                    peerMessageSenderApi = peerMsgSender,
                    chainApi = chainApi,
                    stopBlockHeaderDb = bestBlockHeader,
                    state = fhs)
                  .map(_ => ())
              case x @ (_: FilterSync | _: HeaderSync) =>
                val exn = new RuntimeException(
                  s"Invalid state to start syncing filter headers with, got=$x")
                Future.failed(exn)
            }

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
                                 compactFilterStartHeightOpt = None,
                                 nodeState = nodeState)
            }
          case (Some(bestFilterHeader), None) =>
            val compactFilterStartHeightOptF =
              PeerManager.getCompactFilterStartHeight(chainApi,
                                                      walletCreationTimeOpt)
            for {
              compactFilterStartHeightOpt <- compactFilterStartHeightOptF
              _ <- syncCompactFilters(bestFilterHeader = bestFilterHeader,
                                      chainApi = chainApi,
                                      compactFilterStartHeightOpt =
                                        compactFilterStartHeightOpt,
                                      nodeState = nodeState)
            } yield ()

        }
      }
    }
  }

  /** Attempts to start syncing from a new peer. Returns None if we have no new peers to sync with */
  private def syncFromNewPeer(
      state: NodeRunningState): Future[Option[NodeRunningState]] = {
    val svcIdentifier = ServiceIdentifier.NODE_COMPACT_FILTERS
    val syncPeerOpt = state match {
      case s: SyncNodeState =>
        s.randomPeer(excludePeers = Set(s.syncPeer), svcIdentifier)
      case m: MisbehavingPeer =>
        m.randomPeer(excludePeers = Set(m.badPeer), svcIdentifier)
      case rm: RemovePeers =>
        rm.randomPeer(excludePeers = rm.peersToRemove.toSet, svcIdentifier)
      case d: DoneSyncing =>
        d.randomPeer(Set.empty, svcIdentifier)
      case _: NodeShuttingDown => None
    }
    val newStateOptF: Future[Option[NodeRunningState]] = for {
      newStateOpt <- syncPeerOpt match {
        case Some(syncPeer) =>
          state match {
            case sns: SyncNodeState =>
              val newState = sns.replaceSyncPeer(syncPeer)
              syncHelper(newState).map(_ => Some(newState))
            case d: DoneSyncing =>
              val hs = HeaderSync(syncPeer,
                                  d.peerDataMap,
                                  d.waitingForDisconnection,
                                  d.peerFinder)
              syncHelper(hs).map(_ => Some(hs))
            case x @ (_: MisbehavingPeer | _: RemovePeers |
                _: NodeShuttingDown) =>
              Future.successful(Some(x))
          }
        case None => Future.successful(None)
      }
    } yield newStateOpt

    newStateOptF
  }

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  override def gossipMessage(
      msg: NetworkPayload,
      excludedPeerOpt: Option[Peer]): Future[Unit] = {
    val m = NetworkMessage(chainAppConfig.network, msg)
    queue
      .offer(GossipMessage(m, excludedPeerOpt))
      .map(_ => ())
  }

  override def gossipGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit] = {
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101).map(_.flip))
    gossipMessage(msg = headersMsg, excludedPeerOpt = None)
  }

  override def sendToRandomPeer(payload: NetworkPayload): Future[Unit] = {
    val msg = NetworkMessage(nodeAppConfig.network, payload)
    val stp = SendToPeer(msg, None)
    queue
      .offer(stp)
      .map(_ => ())
  }
}

case class ResponseTimeout(payload: NetworkPayload)

object PeerManager extends Logging {

  /** Sends first getcfheader message.
    * Returns None if are our filter headers are in sync with our block headers
    */
  def sendFirstGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      stopBlockHeaderDb: BlockHeaderDb,
      state: SyncNodeState)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[
    Option[NodeState.FilterHeaderSync]] = {
    for {
      bestFilterHeaderOpt <-
        chainApi
          .getBestFilterHeader()
      blockHash = bestFilterHeaderOpt match {
        case Some(filterHeaderDb) =>
          //need to check for reorg scenarios here
          val isSameHeight = filterHeaderDb.height == stopBlockHeaderDb.height
          val isNotSameBlockHash =
            filterHeaderDb.blockHashBE != stopBlockHeaderDb.hashBE
          if (isSameHeight && isNotSameBlockHash) {
            //need to start from previous header has to sync filter headers
            //correctly in a reorg scenario
            stopBlockHeaderDb.previousBlockHashBE
          } else {
            filterHeaderDb.blockHashBE
          }

        case None =>
          DoubleSha256DigestBE.empty
      }
      hashHeightOpt <- {
        chainApi.nextBlockHeaderBatchRange(prevStopHash = blockHash,
                                           stopHash = stopBlockHeaderDb.hashBE,
                                           batchSize =
                                             chainConfig.filterHeaderBatchSize)
      }
      //needed to work around this bug in bitcoin core:
      //https://github.com/bitcoin/bitcoin/issues/27085
      _ <- AsyncUtil.nonBlockingSleep(1.second)
      res <- hashHeightOpt match {
        case Some(filterSyncMarker) =>
          peerMessageSenderApi
            .sendGetCompactFilterHeadersMessage(filterSyncMarker)
            .map(_ =>
              Some(
                FilterHeaderSync(syncPeer = state.syncPeer,
                                 peerDataMap = state.peerDataMap,
                                 waitingForDisconnection =
                                   state.waitingForDisconnection,
                                 state.peerFinder)))
        case None =>
          logger.info(
            s"Filter headers are synced! filterHeader.blockHashBE=$blockHash")
          Future.successful(None)
      }
    } yield res
  }

  def sendNextGetCompactFilterHeadersCommand(
      peerMessageSenderApi: PeerMessageSenderApi,
      chainApi: ChainApi,
      peer: Peer,
      filterHeaderBatchSize: Int,
      prevStopHash: DoubleSha256DigestBE,
      stopHash: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <- chainApi.nextBlockHeaderBatchRange(
        prevStopHash = prevStopHash,
        stopHash = stopHash,
        batchSize = filterHeaderBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(
            s"Requesting next compact filter headers from $filterSyncMarker with peer=$peer")
          peerMessageSenderApi
            .sendGetCompactFilterHeadersMessage(filterSyncMarker)
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
      startHeightOpt: Option[Int],
      stopBlockHash: DoubleSha256DigestBE,
      peer: Peer)(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <-
        chainApi.nextFilterHeaderBatchRange(stopBlockHash = stopBlockHash,
                                            batchSize = filterBatchSize,
                                            startHeightOpt = startHeightOpt)
      _ = logger.info(
        s"Requesting compact filters from $filterSyncMarkerOpt with peer=$peer startHeightOpt=$startHeightOpt stopBlockHashBE=$stopBlockHash")
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          peerMessageSenderApi
            .sendGetCompactFiltersMessage(filterSyncMarker)
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  def fetchCompactFilterHeaders(
      state: SyncNodeState, //can we tighten this type up?
      chainApi: ChainApi,
      peerMessageSenderApi: PeerMessageSenderApi,
      stopBlockHeaderDb: BlockHeaderDb)(implicit
      ec: ExecutionContext,
      chainAppConfig: ChainAppConfig): Future[
    Option[NodeState.FilterHeaderSync]] = {
    logger.info(
      s"Now syncing filter headers from ${state.syncPeer} in state=${state} stopBlockHashBE=${stopBlockHeaderDb.hashBE}")
    for {
      newSyncingStateOpt <- PeerManager.sendFirstGetCompactFilterHeadersCommand(
        peerMessageSenderApi = peerMessageSenderApi,
        chainApi = chainApi,
        stopBlockHeaderDb = stopBlockHeaderDb,
        state = state)
    } yield {
      newSyncingStateOpt
    }
  }

  def isFiltersOutOfSync(
      blockCount: Int,
      oldFilterHeaderCount: Int,
      currentFilterHeaderCount: Int,
      oldFilterCount: Int,
      currentFilterCount: Int): Boolean = {
    (oldFilterHeaderCount == currentFilterHeaderCount && oldFilterCount == currentFilterCount) &&
    (blockCount != currentFilterHeaderCount || blockCount != currentFilterCount)
  }

  /** Return the starting point for first sync of compact filters from the network
    *
    * @param chainApi
    * @param walletCreationTimeOpt the time the wallet was created, we will start syncing form this point if given
    * @param ec
    * @return the start height for compact filters
    */
  def getCompactFilterStartHeight(
      chainApi: ChainApi,
      walletCreationTimeOpt: Option[Instant])(implicit
      ec: ExecutionContext): Future[Option[Int]] = {
    chainApi.getBestFilter().flatMap {
      case Some(_) =>
        //we have already started syncing filters, return the height of the last filter seen
        Future.successful(None)
      case None =>
        walletCreationTimeOpt match {
          case Some(instant) =>
            val creationTimeHeightF = chainApi
              .epochSecondToBlockHeight(instant.toEpochMilli / 1000)
            val filterCountF = chainApi.getFilterCount()
            for {
              creationTimeHeight <- creationTimeHeightF
              filterCount <- filterCountF
            } yield {
              //filterHeightOpt contains the height of the last filter of the last batch
              //so if we want to start syncing filters from the correct height we need to
              //decrease the computed height
              val height = Math.max(0, creationTimeHeight - 1)
              //want to choose the maximum out of these too
              //if our internal chainstate filter count is > creationTimeHeight
              //we just want to start syncing from our last seen filter
              val result = Math.max(height, filterCount)
              Some(result)
            }
          case None =>
            Future.successful(None)
        }
    }
  }

  def handleHealthCheck(
      runningState: NodeRunningState): Future[NodeRunningState] = {
    val blockFilterPeers = runningState.peerDataMap.filter(
      _._2.serviceIdentifier.hasServicesOf(
        ServiceIdentifier.NODE_COMPACT_FILTERS))
    if (blockFilterPeers.nonEmpty) {
      //do nothing
      Future.successful(runningState)
    } else {
      val peerFinder = runningState.peerFinder
      peerFinder.queryForPeerConnections(excludePeers = Set.empty)
      Future.successful(runningState)
    }
  }
}
