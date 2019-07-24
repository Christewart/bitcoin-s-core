package org.bitcoins.chain.blockchain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    chainConfig: ChainAppConfig,
    blockchains: Vector[Blockchain])
    extends ChainApi
    with BitcoinSLogger {

  override def getBlockCount(implicit ec: ExecutionContext): Future[Long] = {
    logger.debug(s"Querying for block count")
    blockHeaderDAO.maxHeight.map { height =>
      logger.debug(s"getBlockCount result: count=$height")
      height
    }
  }

  override def getHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash).map { header =>
      logger.debug(s"Looking for header by hash=$hash")
      val resultStr = header
        .map(h => s"height=${h.height}, hash=${h.hashBE}")
        .getOrElse("None")
      logger.debug(s"getHeader result: $resultStr")
      header
    }
  }

  override def processHeader(header: BlockHeader)(
      implicit ec: ExecutionContext): Future[ChainHandler] = {

    val blockchainUpdateF = Blockchain.connectTip(header = header,
                                                  blockHeaderDAO =
                                                    blockHeaderDAO,
                                                  blockchains = blockchains)

    val newHandlerF = blockchainUpdateF.flatMap {
      case BlockchainUpdate.Successful(newChain, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map { header =>
          logger.debug(
            s"Connected new header to blockchain, height=${header.height} hash=${header.hashBE}")
          val chainIdxOpt = blockchains.zipWithIndex.find {
            case (chain, _) =>
              newChain.secondTip == chain.tip && blockchains.length == 1
          }

          val updatedChains = {
            chainIdxOpt match {
              case Some((_, idx)) =>
                logger.info(
                  s"Updating chain at idx=${idx} out of ${blockchains.length} with new tip=${header.hashBE.hex}")
                blockchains.updated(idx, newChain)

              case None =>
                logger.info(
                  s"New competing blockchain with tip=${newChain.tip}")
                blockchains.:+(newChain)
            }
          }

          ChainHandler(blockHeaderDAO, chainConfig, updatedChains)

        }
      case BlockchainUpdate.Failed(_, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        Future.failed(new RuntimeException(errMsg))
    }

    blockchainUpdateF.failed.foreach { err =>
      logger.error(
        s"Failed to connect header=${header.hashBE.hex} err=${err.getMessage}")

    }

    newHandlerF
  }

  /**
    * @inheritdoc
    */
  override def getBestBlockHash(
      implicit ec: ExecutionContext): Future[DoubleSha256DigestBE] = {
    logger.debug(s"Querying for best block hash")
    //naive implementation, this is looking for the tip with the _most_ proof of work
    //this does _not_ mean that it is on the chain that has the most work
    //TODO: Enhance this in the future to return the "heaviest" header
    //https://bitcoin.org/en/glossary/block-chain
    val groupedChains = blockchains.groupBy(_.tip.height)
    val maxHeight = groupedChains.keys.max
    val chains = groupedChains(maxHeight)
    logger.info(s"Chains=${chains.map(_.tip.hashBE.hex)}")
    val hashBE: DoubleSha256DigestBE = groupedChains(maxHeight).head.tip.hashBE
    Future.successful(hashBE)
  }
}

object ChainHandler {

  def apply(blockHeaderDAO: BlockHeaderDAO, chainConfig: ChainAppConfig)(
      implicit ec: ExecutionContext): Future[ChainHandler] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(
      chains =>
        new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                         chainConfig = chainConfig,
                         blockchains = chains))
  }

  def apply(
      blockHeaderDAO: BlockHeaderDAO,
      chainConfig: ChainAppConfig,
      blockchains: Blockchain): ChainHandler = {
    new ChainHandler(blockHeaderDAO, chainConfig, Vector(blockchains))
  }
}
