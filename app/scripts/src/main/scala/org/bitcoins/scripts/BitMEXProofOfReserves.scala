package org.bitcoins.scripts

import akka.actor.ActorSystem
import akka.stream.IOResult
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.scripts.bitmex.{BitmexProof, JBitmexProof}
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import java.nio.file.Paths
import scala.concurrent.Future

class BitMEXProofOfReserves()(implicit
    override val system: ActorSystem,
    rpcAppConfig: BitcoindRpcAppConfig)
    extends BitcoinSRunner[Unit] {

  override def start(): Future[Unit] = {
    //val bitcoindF = rpcAppConfig.clientF
    val fileName =
      "/home/chris/Downloads/20221109-reserves-762408-20221109D113037689340000.yaml"
    val proofOfReservesFile = Paths.get(fileName)
    val source: Source[ByteString, Future[IOResult]] =
      FileIO.fromPath(proofOfReservesFile)
    val mapper: ObjectMapper = new YAMLMapper()
    val jproof =
      mapper.readValue(proofOfReservesFile.toFile, classOf[JBitmexProof])
    val proof = BitmexProof(jproof)
    println(s"proof=$proof")
    Future.unit
  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
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
