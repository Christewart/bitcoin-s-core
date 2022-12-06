package org.bitcoins.server.routes

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.core.util.{EnvUtil, StartStopAsync}

import scala.concurrent.{ExecutionContext, Future}

trait BitcoinSRunner[T] extends StartStopAsync[T] with Logging {

  implicit def system: ActorSystem

  implicit lazy val ec: ExecutionContext = system.dispatcher

  // start everything!
  final def run(): Future[T] = {

    //We need to set the system property before any logger instances
    //are in instantiated. If we don't do this, we will not log to
    //the correct location
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/2496
    //System.setProperty("bitcoins.log.location", usedDir.toAbsolutePath.toString)

    logger.info(
      s"version=${EnvUtil.getVersion} jdkVersion=${EnvUtil.getJdkVersion}")

    //logger.info(s"using directory ${usedDir.toAbsolutePath.toString}")
    val runner: Future[T] = start()
    runner.failed.foreach { err =>
      logger.error(s"Failed to startup server!", err)
    }(scala.concurrent.ExecutionContext.Implicits.global)

    runner.flatMap(_ => stop())
  }
}

trait BitcoinSServerRunner[T] extends BitcoinSRunner[T] {
  protected def serverArgParser: ServerArgParser
}
