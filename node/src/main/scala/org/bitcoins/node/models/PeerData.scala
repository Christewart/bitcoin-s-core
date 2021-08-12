package org.bitcoins.node.models

import akka.actor.ActorSystem
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.node.Node
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageSender
}

case class PeerData(peer: Peer, node: Node)(implicit
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig) {

  lazy val peerMessageSender: PeerMessageSender = PeerMessageSender(client)

  lazy val client: P2PClient = {
    val peerMessageReceiver =
      PeerMessageReceiver.newReceiver(node = node, peer = peer)
    P2PClient(context = system,
              peer = peer,
              peerMessageReceiver = peerMessageReceiver)
  }

  private var _serviceIdentifier: Option[ServiceIdentifier] = None

  def serviceIdentifier: ServiceIdentifier = {
    _serviceIdentifier.getOrElse(
      throw new RuntimeException("Service identifier not initialized"))
  }

  def setServiceIdentifier(serviceIdentifier: ServiceIdentifier): Unit = {
    _serviceIdentifier = Some(serviceIdentifier)
  }
}
