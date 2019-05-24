package org.bitcoins.node.networking.peer

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.db.AppConfig
import org.bitcoins.node.messages.data.GetHeadersMessage
import org.bitcoins.node.messages.{
  DataPayload,
  HeadersMessage,
  InventoryMessage,
  TypeIdentifier
}

import scala.concurrent.{ExecutionContext, Future}

/** This actor is meant to handle a [[org.bitcoins.node.messages.DataPayload]]
  * that a peer to sent to us on the p2p network, for instance, if we a receive a
  * [[HeadersMessage]] we should store those headers in our database
  */
class DataMessageHandler(appConfig: AppConfig)(implicit ec: ExecutionContext)
    extends BitcoinSLogger {

  private val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO(appConfig)

  def handleDataPayload(
      payload: DataPayload,
      peerMsgSender: PeerMessageSender): Future[Unit] = payload match {
    case headersMsg: HeadersMessage =>
      val headers = headersMsg.headers
      val chainApi: ChainApi =
        ChainHandler(blockHeaderDAO, chainConfig = appConfig)
      val chainApiF = chainApi.processHeaders(headers)

      chainApiF.map { chainApi =>
        val lastHash = headers.last.hash
        peerMsgSender.sendGetHeadersMessage(lastHash)
      }
    case invMsg: InventoryMessage =>
      handleInventoryMsg(invMsg = invMsg, peerMsgSender = peerMsgSender)
  }

  private def handleInventoryMsg(
      invMsg: InventoryMessage,
      peerMsgSender: PeerMessageSender): Future[Unit] = {
    logger.info(s"Received inv=${invMsg}")
    /*    val invs: Seq[Future[Unit]] = invMsg.inventories.map { inv =>
      inv.typeIdentifier match {
        case TypeIdentifier.MsgBlock =>
          val hash = inv.hash
          peerMsgSender.sendGetHeadersMessage(hash)
          FutureUtil.unit
      }
    }

    Future.sequence(invs).map(_ => ())*/

    FutureUtil.unit

  }
}
