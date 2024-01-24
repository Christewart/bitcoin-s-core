package org.bitcoins.scripts

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, Sink}
import org.bitcoins.server.routes.BitcoinSRunner
import org.bitcoins.server.util.BitcoinSAppScalaDaemon

import scala.concurrent.Future

case class ComputeSizeIncrease()(implicit override val system: ActorSystem)
    extends BitcoinSRunner[Unit] {

  override def start(): Future[Unit] = {
    import ScriptNumHelper.scriptNumHelperRw

    val parseFlow = Flow.fromFunction { str: String =>
      val drop = str.dropRight(1)
      upickle.default.read[ScriptNumHelper](drop)
    }
    val sink: Sink[ScriptNumHelper, Future[Long]] = Sink.fold(0L) {
      case (acc, scriptNumHelper) =>
        acc + scriptNumHelper.sizeIncrease
    }

    val sizeIncreaseF: Future[Long] = ScriptNumHelper.source
      .via(parseFlow)
      .toMat(sink)(Keep.right)
      .run()

    sizeIncreaseF.map { sizeIncrease =>
      logger.info(s"Size increase=${sizeIncrease}")
    }

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
