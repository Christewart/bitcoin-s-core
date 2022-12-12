package org.bitcoins.scripts

import akka.actor.ActorSystem
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.crypto.{
  CryptoUtil,
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  Sha256Digest
}
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.scripts.bitmex.{
  BitmexLiabilities,
  BitmexProof,
  BitmexUserLiability,
  JBitmexProof
}
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon
import scodec.bits.ByteVector

import java.nio.file.Paths
import scala.concurrent.Future

class BitMEXProofOfReserves()(implicit
    override val system: ActorSystem,
    rpcAppConfig: BitcoindRpcAppConfig)
    extends BitcoinSRunner[Unit] {

  //from: https://blog.bitmex.com/proof-of-reserves-liabilities-bitmex-demonstration/
  private val insuranceFundNonce = {
    ByteVector.fromValidHex(
      "d25a3b153f6a66d77077a195de783bddd58625350482bf874d4439e7f766c6d4")
  }

  private val insuranceFundAccountNumber: BigInt = BigInt(
    "18446744073709551615")

  override def start(): Future[Unit] = {
    val reserveFileName =
      "/home/chris/Downloads/20221109-reserves-762408-20221109D113037689340000.yaml"
    val proofOfReservesFile = Paths.get(reserveFileName)

    val mapper: ObjectMapper = new YAMLMapper()
    val proofF = Future {
      val jproof =
        mapper.readValue(proofOfReservesFile.toFile, classOf[JBitmexProof])
      val proof = BitmexProof(jproof)
      proof
    }

    val liabilitiesF = parseLiabilityFile()

    for {
      proof <- proofF
      liabilities <- liabilitiesF
      insuranceFundSubNonce = getInsuranceFundSubNonce(liabilities.blockHeight)
      insuranceFundLeafHash = getMerkleTreeLeafHash(
        accountId = insuranceFundAccountNumber,
        subNonce = insuranceFundSubNonce,
        balance = ???,
        leafPositionIndex = ???)
    } yield {
      println(s"proof=$proof")
      println(s"liabilities=$liabilities")
      println(s"insuranceFundSubNonce=${insuranceFundSubNonce.hex}")
      ()
    }
  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
  }

  private def parseLiabilityFile(): Future[BitmexLiabilities] = {
    Future {
      val liabilityFileName =
        "/home/chris/Downloads/20221109-liabilities-762408-20221109D101503.455087000.csv"
      val proofOfLiabilityFile = scala.io.Source.fromFile(liabilityFileName)
      val lines = proofOfLiabilityFile.getLines()
      //1st line is block height
      val height: Int = {
        lines.take(1).toVector.head.dropWhile(char => !char.isDigit).toInt
      }

      val liabilityLines = proofOfLiabilityFile.getLines().drop(1)
      val liabilities = liabilityLines.map { line =>
        val split = line.split(",")
        val hash = DoubleSha256DigestBE.fromHex(split.head)
        val balance = Satoshis(split(1).toLong)
        BitmexUserLiability(hash, balance)
      }

      BitmexLiabilities(height, liabilities.toVector)
    }
  }

  def getInsuranceFundSubNonce(blockHeight: Long): Sha256Digest = {
    val bytes = insuranceFundNonce ++ ByteVector.fromLong(blockHeight)
    CryptoUtil.sha256(bytes)
  }

  def getMerkleTreeLeafHash(
      accountId: BigInt,
      subNonce: Sha256Digest,
      balance: Satoshis,
      leafPositionIndex: Int): Sha256Digest = {
    val bytes = ByteVector(accountId.toByteArray) ++ subNonce.bytes ++
      balance.bytes ++ ByteVector.fromInt(leafPositionIndex)

    CryptoUtil.sha256(bytes)
  }
}

object BitMEXProofOfReserves extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"bitmex-por-pol-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig.fromDefaultDatadir()(system)

  new BitMEXProofOfReserves().run()
}
