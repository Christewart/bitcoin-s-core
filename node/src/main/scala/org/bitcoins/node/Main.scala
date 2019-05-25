package org.bitcoins.node

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.db.AppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.util.NetworkIpAddress

/**
  * Created by chris on 8/29/16.
  */
object Main extends App {

  override def main(args: Array[String]) = {
    implicit val system: ActorSystem = ActorSystem(
      s"spv-node-${System.currentTimeMillis()}")
    import system.dispatcher
    implicit val appconfig = NodeAppConfig
    val chainAppConfig = ChainAppConfig
    val bhDAO = BlockHeaderDAO(chainAppConfig)
    val chainApi = ChainHandler(bhDAO, chainAppConfig)

    val socket = new InetSocketAddress("localhost", 18333)
    val nip = NetworkIpAddress.fromInetSocketAddress(socket)
    val peer = Peer(None, nip)
    val spvNode = SpvNode(peer, chainApi)

    spvNode.sync()
  }

}
