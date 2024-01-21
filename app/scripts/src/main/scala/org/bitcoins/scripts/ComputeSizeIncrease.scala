package org.bitcoins.scripts

import akka.actor.ActorSystem
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import scala.concurrent.Future

case class ComputeSizeIncrease()(implicit override val system: ActorSystem)
    extends BitcoinSRunner[Unit] {

  override def start(): Future[Unit] = {
    import ScriptNumHelper.scriptNumHelperRw
    val json = ujson.read(ScriptNumHelper.path)
    val scriptNums: Seq[ScriptNumHelper] =
      upickle.default.read[Seq[ScriptNumHelper]](json)

    val sizeIncrease = scriptNums.foldLeft(0L) { case (accum, helper) =>
      accum + helper.sizeIncrease
    }

    logger.info(s"Size increase=${sizeIncrease}")
    Future.unit

  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
  }
}

object ComputeSizeIncrease extends BitcoinSAppScalaDaemon {

  override val actorSystemName: String =
    s"scan-bitcoind-${System.currentTimeMillis()}"

  override val customFinalDirOpt = None

  new ComputeSizeIncrease().run()
}
