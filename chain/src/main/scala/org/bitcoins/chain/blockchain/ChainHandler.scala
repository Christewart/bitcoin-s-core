package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models._
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.gcs.{BlockFilter, FilterHeader, GolombFilter}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.CryptoUtil

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  *
  * @param blockHeaderDAO block header DB
  * @param filterHeaderDAO filter header DB
  * @param filterDAO filter DB
  * @param blockchains current blockchains
  * @param blockFilterCheckpoints compact filter checkpoints for filter header verification
  * @param chainConfig config file
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    filterHeaderDAO: CompactFilterHeaderDAO,
    filterDAO: CompactFilterDAO,
    blockchains: Vector[Blockchain],
    blockFilterCheckpoints: Map[DoubleSha256DigestBE, DoubleSha256DigestBE])(
    implicit private[chain] val chainConfig: ChainAppConfig)
    extends ChainApi
    with ChainVerificationLogger {

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

  override def getNthHeader(hash: DoubleSha256DigestBE, count: Int)(
    implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
    val range = 0.until(count)
    range.foldLeft(getHeader(hash)) { (headerF, _) =>
      headerF.flatMap {
        case Some(header) => getHeader(header.previousBlockHashBE)
        case None => headerF
      }
    }
  }

  /** @inheritdoc */
  override def processHeaders(headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    val blockchainUpdates: Vector[BlockchainUpdate] = {
      Blockchain.connectHeadersToChains(headers, blockchains)
    }

    val headersToBeCreated = {
      blockchainUpdates.flatMap(_.successfulHeaders).distinct
    }

    val chains = blockchainUpdates.map(_.blockchain)

    val createdF = blockHeaderDAO.createAll(headersToBeCreated)

    val newChainHandler = this.copy(blockchains = chains)

    createdF.map { _ =>
      newChainHandler
    }
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

    val hashBE: DoubleSha256DigestBE = chains match {
      case Vector() =>
        val errMsg = s"Did not find blockchain with height $maxHeight"
        logger.error(errMsg)
        throw new RuntimeException(errMsg)
      case chain +: Vector() =>
        chain.tip.hashBE
      case chain +: rest =>
        logger.warn(
          s"We have multiple competing blockchains: ${(chain +: rest).map(_.tip.hashBE.hex).mkString(", ")}")
        chain.tip.hashBE
    }
    Future.successful(hashBE)
  }

  override def nextBatchRange(prevStopHash: DoubleSha256DigestBE, batchSize: Long)(implicit ec: ExecutionContext): Future[Option[(Int, DoubleSha256Digest)]] = {
    val startHeightF = if (prevStopHash == DoubleSha256DigestBE.empty) {
      Future.successful(0)
    } else {
      for {
        prevStopHeaderOpt <- getHeader(prevStopHash)
        prevStopHeader = prevStopHeaderOpt.getOrElse(throw new RuntimeException(s"Unknown block hash ${prevStopHash}"))
      } yield prevStopHeader.height + 1
    }
    for {
      startHeight <- startHeightF
      blockCount <- getBlockCount
      stopHeight = if (startHeight - 1 + batchSize > blockCount) blockCount else startHeight - 1 + batchSize
      stopBlockOpt <- getHeadersByHeight(stopHeight.toInt).map(_.headOption)
      stopBlock = stopBlockOpt.getOrElse(throw new RuntimeException(s"Unknown header height ${stopHeight}"))
    } yield {
      if (startHeight > stopHeight)
        None
      else
        Some((startHeight, stopBlock.hashBE.flip))
    }
  }

  override def processFilterHeader(
      filterHeader: FilterHeader,
      blockHash: DoubleSha256DigestBE,
      height: Int)(implicit ec: ExecutionContext): Future[ChainApi] = {
    val filterHeaderDb = CompactFilterHeaderDbHelper.fromFilterHeader(
      filterHeader,
      blockHash,
      height)

    def validateAndInsert(
        filterHeaderDbOpt: Option[CompactFilterHeaderDb]): Future[
      CompactFilterHeaderDb] = {
      filterHeaderDbOpt match {
        case Some(found) =>
          if (found != filterHeaderDb) {
            val errMsg =
              s"We have a conflicting compact filter header (${filterHeaderDb.hashBE}) in the DB"
            Future.failed(new RuntimeException(errMsg))
          } else {
            logger.debug(s"We have already processed filter header=${found.hashBE}")
            Future.successful(filterHeaderDb)
          }
        case None =>
          filterHeaderDAO.create(filterHeaderDb)
      }
    }

    for {
      blockHeaderOpt <- blockHeaderDAO.findByHash(filterHeaderDb.blockHashBE)
      _ = blockHeaderOpt.getOrElse(
        throw new RuntimeException(s"Unknown block ${blockHash}"))
      filterHeaderDbOpt <- filterHeaderDAO.findByHash(filterHeaderDb.hashBE)
      _ <- validateAndInsert(filterHeaderDbOpt)
    } yield this
  }

  override def processFilter(
      message: CompactFilterMessage,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {

    val filterHashBE = CryptoUtil.doubleSHA256(message.filterBytes).flip

    def validateAndInsert(
        filterHeader: CompactFilterHeaderDb,
        filterOpt: Option[CompactFilterDb]): Future[CompactFilterDb] = {
      if (filterHashBE != filterHeader.filterHashBE) {
        val errMsg = s"Filter hash does not match filter header hash: ${filterHashBE} != ${filterHeader.filterHashBE}\n" +
        s"filter=${message.filterBytes.toHex}\nblock hash=${message.blockHash}\nfilterHeader=${filterHeader}"
        logger.warn(errMsg)
      }
      filterOpt match {
        case Some(filter) =>
          val filterDb = CompactFilterDbHelper.fromFilterBytes(message.filterBytes, filterHeader.blockHashBE, filterHeader.height)
          if (filterDb != filter) {
            val errMsg = s"Filter does not match: ${filterDb} != ${filter}\n" +
              s"filter=${message.filterBytes.toHex}\nblock hash=${message.blockHash}"
            logger.warn(errMsg)
            for {
              res <- filterDAO.update(filterDb)
            } yield res
          } else {
            logger.debug(s"We have already processed filter=${filter.hashBE}")
            Future.successful(filter)
          }
        case None =>
          val filterDb = CompactFilterDbHelper.fromFilterBytes(message.filterBytes, filterHeader.blockHashBE, filterHeader.height)
          val golombFilter = BlockFilter.fromBytes(message.filterBytes, message.blockHash)
          val filterDb1 = CompactFilterDbHelper.fromGolombFilter(golombFilter, filterHeader.blockHashBE, filterHeader.height)
          if (filterDb != filterDb1) {
            val errMsg = s"Golomb filter does not match: ${filterDb} != ${filterDb1}\n" +
              s"filter=${message.filterBytes.toHex}\nblock hash=${message.blockHash}"
            logger.warn(errMsg)
          }
          for {
            res <- filterDAO.create(filterDb)
          } yield res
      }
    }

    for {
      filterHeaderOpt <- filterHeaderDAO.findByBlockHash(blockHash)
      filterHeader = filterHeaderOpt.getOrElse(
        throw new RuntimeException(
          s"Cannot find a filter header for block hash ${blockHash}"))
      filterOpt <- filterDAO.findByBlockHash(blockHash)
      _ <- validateAndInsert(filterHeader, filterOpt)
    } yield {
      this
    }
  }

  override def processCheckpoint(
      filterHeaderHash: DoubleSha256DigestBE,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {
      blockFilterCheckpoints.get(blockHash) match {
        case Some(oldFilterHeaderHash) =>
          if (filterHeaderHash != oldFilterHeaderHash)
            Future.failed(new RuntimeException(
              "The peer sent us a different filter header hash"))
          else
            Future.successful(this.copy(
              blockFilterCheckpoints =
                blockFilterCheckpoints.updated(blockHash, filterHeaderHash)))
        case None =>
          Future.successful(this)
      }

  }

  override def getHighestFilterHeader(
      implicit ec: ExecutionContext): Future[Option[CompactFilterHeaderDb]] = {
    filterHeaderDAO.findHighest()
  }

  override def getFilterHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[FilterHeader]] = {
    filterHeaderDAO.findByHash(hash).map(_.map(x => x.filterHeader))
  }

  override def getHighestFilter(implicit ec: ExecutionContext): Future[Option[CompactFilterDb]] = {
    filterDAO.findHighest()
  }

  override def getFilter(blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[CompactFilterDb]] = {
    filterDAO.findByBlockHash(blockHash)
  }

  override def getHeadersByHeight(height: Int)(
    implicit ec: ExecutionContext): Future[Seq[BlockHeaderDb]] = {
    blockHeaderDAO.findByHeight(height)
  }
}

object ChainHandler {

  /** Constructs a [[ChainHandler chain handler]] from the state in the database
    * This gives us the guaranteed latest state we have in the database
    * */
  def fromDatabase(
      blockHeaderDAO: BlockHeaderDAO,
      filterHeaderDAO: CompactFilterHeaderDAO,
      filterDAO: CompactFilterDAO)(
      implicit ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[ChainHandler] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(
      chains =>
        new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                         filterHeaderDAO = filterHeaderDAO,
                         filterDAO = filterDAO,
                         blockchains = chains,
                         blockFilterCheckpoints = Map.empty))
  }

  def apply(
      blockHeaderDAO: BlockHeaderDAO,
      filterHeaderDAO: CompactFilterHeaderDAO,
      filterDAO: CompactFilterDAO,
      blockchains: Blockchain)(
      implicit chainConfig: ChainAppConfig): ChainHandler = {
    new ChainHandler(blockHeaderDAO = blockHeaderDAO,
                     filterHeaderDAO = filterHeaderDAO,
                     filterDAO = filterDAO,
                     blockchains = Vector(blockchains),
                     blockFilterCheckpoints = Map.empty)
  }
}
