package org.bitcoins.scripts

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Keep, Source}
import akka.util.ByteString
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  EmptyTransaction,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.script.constant.{
  ScriptConstant,
  ScriptNumberOperation,
  ScriptToken
}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import scala.concurrent.Future

/** Useful script for scanning bitcoind
  * This file assumes you have pre-configured the connection
  * between bitcoin-s and bitcoind inside of bitcoin-s.conf
  * @see https://bitcoin-s.org/docs/config/configuration#example-configuration-file
  */
class ScanBitcoind()(implicit
    override val system: ActorSystem,
    rpcAppConfig: BitcoindRpcAppConfig)
    extends BitcoinSRunner[Unit] {

  override def start(): Future[Unit] = {

    val bitcoindF = rpcAppConfig.clientF

    //    val startHeight = 675000
    //val endHeightF: Future[Int] = bitcoindF.flatMap(_.getBlockCount())

    val f = for {
      bitcoind <- bitcoindF
//      endHeight <- endHeightF
      //_ <- countWitV1MempoolTxs(bitcoind)
      _ <- countAllScriptNums(bitcoind)
    } yield ()
    f.failed.foreach(err =>
      logger.error(s"Failed to count witness v1 mempool txs", err))
    Future.unit
  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
  }

  /** Searches a given Source[Int] that represents block heights applying f to them and returning a Seq[T] with the results */
  def searchBlocks[T <: Json](
      bitcoind: BitcoindRpcClient,
      source: Source[Int, NotUsed],
      f: Block => Vector[T],
      file: Path,
      numParallelism: Int = Runtime.getRuntime.availableProcessors()): Future[
    IOResult] = {
    source
      .mapAsync(parallelism = numParallelism) { height =>
        bitcoind
          .getBlockHash(height)
          .flatMap(h => bitcoind.getBlockRaw(h))
          .map(b => (b, height))
      }
      .mapAsync(numParallelism) { case (block, height) =>
        logger.info(
          s"Searching block at height=$height hashBE=${block.blockHeader.hashBE.hex}")
        FutureUtil.makeAsync { () =>
          f(block)
        }
      }
      .mapConcat(identity)
      .map(_.bytes)
      .toMat(FileIO.toPath(file))(Keep.right)
      .run()
  }

//  def countSegwitTxs(
//      bitcoind: BitcoindRpcClient,
//      startHeight: Int,
//      endHeight: Int): Future[Unit] = {
//    val startTime = System.currentTimeMillis()
//    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))
//
//    //in this simple example, we are going to count the number of witness transactions
//    val countSegwitTxs: Block => Int = { block: Block =>
//      block.transactions.count(_.isInstanceOf[WitnessTransaction])
//    }
//    val countsF: Future[Seq[Int]] = for {
//      counts <- searchBlocks[Int](bitcoind, source, countSegwitTxs)
//    } yield counts
//
//    val countF: Future[Int] = countsF.map(_.sum)
//
//    for {
//      count <- countF
//      endTime = System.currentTimeMillis()
//      _ = logger.info(
//        s"Count of segwit txs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms ")
//    } yield ()
//  }
//
//  def countTaprootTxsInBlocks(
//      endHeight: Int,
//      lastBlocks: Int,
//      bitcoind: BitcoindRpcClient): Future[Int] = {
//    val startTime = System.currentTimeMillis()
//    val startHeight = endHeight - lastBlocks
//    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))
//    val countTaprootOutputs: Block => Int = { block =>
//      val outputs = block.transactions
//        .flatMap(_.outputs)
//        .filter(_.scriptPubKey.isInstanceOf[TaprootScriptPubKey])
//      outputs.length
//    }
//
//    val countsF: Future[Seq[Int]] = for {
//      counts <- searchBlocks[Int](bitcoind, source, countTaprootOutputs)
//    } yield counts
//
//    val countF: Future[Int] = countsF.map(_.sum)
//
//    for {
//      count <- countF
//      endTime = System.currentTimeMillis()
//      _ = logger.info(
//        s"Count of taproot outputs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms ")
//    } yield count
//  }
//
//  def countWitV1MempoolTxs(bitcoind: BitcoindRpcClient): Future[Int] = {
//    val memPoolSourceF = getMemPoolSource(bitcoind)
//    val countF = memPoolSourceF.flatMap(_.runFold(0) { case (count, tx) =>
//      count + tx.outputs.count(_.scriptPubKey.isInstanceOf[TaprootScriptPubKey])
//    })
//    countF.foreach(c =>
//      logger.info(
//        s"Found $c mempool transactions with witness v1 outputs at ${Instant.now}"))
//    countF
//  }

  sealed trait Json {
    def toJson: String

    def bytes: ByteString = {
      ByteString(toJson.getBytes(StandardCharsets.UTF_8))
    }
  }

  private case class ScriptNumHelper(
      tx: Transaction,
      scriptConstants: Vector[ScriptConstant],
      comment: String)
      extends Json {

    def toJson: String = {
      val scriptConstantsStr =
        scriptConstants
          .map(s => "\"" + s.toString + "\"")
          .mkString(",")
      val scriptConstantsBytesStr =
        scriptConstants
          .map(s => "\"" + s.bytes.toHex + "\"")
          .mkString(",")
      s"""{
         |"txId" : "${tx.txIdBE.hex}",
         |"scriptConstants": [${scriptConstantsStr}],
         |"scriptConstantsBytes" : [${scriptConstantsBytesStr}],
         |"comment": "$comment"
         |}""".stripMargin
    }
  }

  def countAllScriptNums(bitcoind: BitcoindRpcClient): Future[Unit] = {
    val blockCountF = Future.successful(300000) //bitcoind.getBlockCount()
    val sourceF = blockCountF.map(h => Source((1.until(h))))
    val fn: Block => Vector[ScriptNumHelper] = { block =>
      block.transactions.map(findScriptNum).flatten.toVector
    }

    val fileName = "scriptnumcount.json"
    val path = Paths.get(fileName)
    val countAllF: Future[Unit] = {
      for {
        src <- sourceF
        _ <- searchBlocks(bitcoind, src, fn, path)
      } yield ()
    }

    countAllF
      .map(_ => logger.info(s"Done"))
  }

  private def findScriptNum(tx: Transaction): Vector[ScriptNumHelper] = {
    val scriptSig: Vector[ScriptNumHelper] =
      tx.inputs
        .map(i => searchAsm(tx, i.scriptSignature))
        .toVector
        .flatten
    val spk: Vector[ScriptNumHelper] =
      tx.outputs
        .map(o => searchAsm(tx, o.scriptPubKey))
        .toVector
        .flatten
    val witness: Vector[ScriptNumHelper] = tx match {
      case _: BaseTransaction | EmptyTransaction => Vector.empty //no witnesses
      case wtx: WitnessTransaction =>
        wtx.witness.toVector.flatMap(wit => searchAsm(tx, wit))
    }

    (scriptSig ++ spk ++ witness)
  }

  private def searchAsm(
      tx: Transaction,
      spk: ScriptPubKey): Option[ScriptNumHelper] = {
    spk match {
      case _ @(_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: P2SHScriptPubKey | _: WitnessScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey) =>
        None
      case p2kwt: P2PKWithTimeoutScriptPubKey =>
        Some(ScriptNumHelper(tx, Vector(p2kwt.lockTime), s"SCRIPTPUBKEY"))
      case m: MultiSignatureScriptPubKey =>
        if (!m.maxSigsScriptNumber.isInstanceOf[ScriptNumberOperation]) {
          None
        } else {
          Some(
            ScriptNumHelper(tx, Vector(m.maxSigsScriptNumber), s"SCRIPTPUBKEY"))
        }
      case mwt: MultiSignatureWithTimeoutScriptPubKey =>
        val snh1Opt = searchAsm(tx, mwt.multiSigSPK)
        val snh2Opt = searchAsm(tx, mwt.timeoutSPK)
        (snh1Opt, snh2Opt) match {
          case (Some(snh1), Some(snh2)) =>
            Some(
              ScriptNumHelper(tx,
                              snh1.scriptConstants ++ snh2.scriptConstants,
                              s"SCRIPTPUBKEY"))
          case (Some(snh1), None) => Some(snh1)
          case (None, Some(snh2)) => Some(snh2)
          case (None, None)       => None
        }
      case cltv: CLTVScriptPubKey =>
        val nestedOpt = searchAsm(tx, cltv.nestedScriptPubKey)
        val base = ScriptNumHelper(tx, Vector(cltv.locktime), s"SCRIPTPUBKEY")
        nestedOpt match {
          case Some(nested) =>
            Some(
              base.copy(scriptConstants =
                base.scriptConstants ++ nested.scriptConstants))
          case None => Some(base)
        }
      case csv: CSVScriptPubKey =>
        val nestedOpt = searchAsm(tx, csv.nestedScriptPubKey)
        val base = ScriptNumHelper(tx, Vector(csv.locktime), s"SCRIPTPUBKEY")
        nestedOpt match {
          case Some(nested) =>
            Some(
              base.copy(scriptConstants =
                base.scriptConstants ++ nested.scriptConstants))
          case None => Some(base)
        }

      case cspk: ConditionalScriptPubKey =>
        val snh1Opt = searchAsm(tx, cspk.firstSPK)
        val snh2Opt = searchAsm(tx, cspk.secondSPK)
        (snh1Opt, snh2Opt) match {
          case (Some(snh1), Some(snh2)) =>
            Some(
              ScriptNumHelper(tx,
                              snh1.scriptConstants ++ snh2.scriptConstants,
                              s"SCRIPTPUBKEY"))
          case (Some(snh1), None) => Some(snh1)
          case (None, Some(snh2)) => Some(snh2)
          case (None, None)       => None
        }
      case n: NonStandardScriptPubKey =>
        searchAsm(tx, n.asm.toVector)
    }
  }

  private def searchAsm(
      tx: Transaction,
      scriptSignature: ScriptSignature): Option[ScriptNumHelper] = {
    scriptSignature match {
      case _: P2PKScriptSignature | _: P2PKHScriptSignature |
          EmptyScriptSignature | TrivialTrueScriptSignature |
          _: MultiSignatureScriptSignature =>
        None
      case _: LockTimeScriptSignature => None
      case c: ConditionalScriptSignature =>
        searchAsm(tx, c.nestedScriptSig)
      case n: NonStandardScriptSignature =>
        searchAsm(tx, n.asm.toVector)
      case p2sh: P2SHScriptSignature =>
        val snh1Opt = searchAsm(tx, p2sh.redeemScript)
        val snh2Opt = searchAsm(tx, p2sh.scriptSignatureNoRedeemScript)
        (snh1Opt, snh2Opt) match {
          case (Some(snh1), Some(snh2)) =>
            Some(
              ScriptNumHelper(tx,
                              snh1.scriptConstants ++ snh2.scriptConstants,
                              "SCRIPTSIG"))
          case (Some(snh1), None) => Some(snh1)
          case (None, Some(snh2)) => Some(snh2)
          case (None, None)       => None
        }
    }
  }

  private def searchAsm(
      tx: Transaction,
      scriptWitness: ScriptWitness): Option[ScriptNumHelper] = {
    scriptWitness match {
      case _: P2WPKHWitnessV0 | EmptyScriptWitness | _: TaprootKeyPath => None
      case p2wsh: P2WSHWitnessV0 =>
        val snh1Opt = searchAsm(tx, p2wsh.redeemScript)
        val snh2Opt = searchAsm(tx, p2wsh.scriptSignature)
        (snh1Opt, snh2Opt) match {
          case (Some(snh1), Some(snh2)) =>
            Some(
              ScriptNumHelper(tx,
                              snh1.scriptConstants ++ snh2.scriptConstants,
                              s"SCRIPTWITNESS"))
          case (Some(snh1), None) => Some(snh1)
          case (None, Some(snh2)) => Some(snh2)
          case (None, None)       => None
        }
      case trsp: TaprootScriptPath =>
        searchAsm(tx, trsp.script)
      case _: TaprootUnknownPath => None
    }
  }

  private def searchAsm(
      tx: Transaction,
      asm: Vector[ScriptToken]): Option[ScriptNumHelper] = {
    val filtered =
      asm.filter(a =>
        a.isInstanceOf[ScriptConstant] && a.bytes.size < 8 && !a
          .isInstanceOf[ScriptNumberOperation])
    if (filtered.isEmpty) None
    else
      Some(
        ScriptNumHelper(tx,
                        filtered.map(_.asInstanceOf[ScriptConstant]),
                        "ASM"))
  }

  def getMemPoolSource(
      bitcoind: BitcoindRpcClient): Future[Source[Transaction, NotUsed]] = {
    val mempoolF = bitcoind.getRawMemPool
    val sourceF: Future[Source[DoubleSha256DigestBE, NotUsed]] =
      mempoolF.map(Source(_))

    val mempoolTxSourceF: Future[Source[Transaction, NotUsed]] = {
      sourceF.map { source =>
        source.mapAsync(Runtime.getRuntime.availableProcessors()) { hash =>
          bitcoind
            .getRawTransaction(hash)
            .map(_.hex)
        }
      }
    }

    mempoolTxSourceF
  }
}

object ScanBitcoind extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"scan-bitcoind-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(system)

  new ScanBitcoind().run()
}
