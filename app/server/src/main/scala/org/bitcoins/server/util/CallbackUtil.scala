package org.bitcoins.server.util

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.node._
import org.bitcoins.node.callback.NodeCallbackStreamManager
import org.bitcoins.wallet.Wallet

import scala.concurrent.{ExecutionContext, Future}

object CallbackUtil extends Logging {

  def createNeutrinoNodeCallbacksForWallet(wallet: Wallet)(implicit
      system: ActorSystem): Future[NodeCallbackStreamManager] = {
    import system.dispatcher
    val nodeCallbacks = buildNodeCallbacks(wallet)
    val manager = NodeCallbackStreamManager(nodeCallbacks)
    Future.successful(manager)
  }

  def createBitcoindNodeCallbacksForWallet(wallet: Wallet)(implicit
      system: ActorSystem): Future[NodeCallbackStreamManager] = {
    import system.dispatcher
    val nodeCallbacks = buildNodeCallbacks(wallet)
    val manager = NodeCallbackStreamManager(nodeCallbacks)
    Future.successful(manager)
  }

  private def buildNodeCallbacks(wallet: Wallet)(implicit
      ec: ExecutionContext): NodeCallbacks = {
    val onTx: OnTxReceived = { tx =>
      logger.info(s"Receiving transaction txid=${tx.txIdBE.hex} as a callback")
      wallet.processTransaction(tx, blockHashOpt = None).map(_ => ())
    }
    val onCompactFilters: OnCompactFiltersReceived = { blockFilters =>
      logger.info(
        s"Executing onCompactFilters callback with filter count=${blockFilters.length}")
      wallet
        .processCompactFilters(blockFilters = blockFilters)
        .map(_.updateUtxoPendingStates())
        .map(_ => ())
    }
    val onBlock: OnBlockReceived = { block =>
      logger.info(s"Executing onBlock callback=${block.blockHeader.hashBE.hex}")
      wallet.processBlock(block).map(_ => ())
    }
    val onHeaders: OnBlockHeadersReceived = { headers =>
      logger.info(s"Executing block header with header count=${headers.length}")
      if (headers.isEmpty) {
        Future.unit
      } else {
        wallet.updateUtxoPendingStates().map(_ => ())
      }
    }

    NodeCallbacks(onTxReceived = Vector(onTx),
                  onBlockReceived = Vector(onBlock),
                  onCompactFiltersReceived = Vector(onCompactFilters),
                  onBlockHeadersReceived = Vector(onHeaders))
  }
}
