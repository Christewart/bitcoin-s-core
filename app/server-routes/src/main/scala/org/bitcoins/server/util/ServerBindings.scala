package org.bitcoins.server.util

import akka.http.scaladsl.Http
import grizzled.slf4j.Logging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

case class ServerBindings(
    httpServer: Http.ServerBinding,
    webSocketServer: Http.ServerBinding)
    extends Logging {

  def stop()(implicit ec: ExecutionContext): Future[Unit] = {
    val stopHttp = httpServer.terminate(3.seconds)
    val stopWs = webSocketServer.terminate(3.seconds)

    logger.info(s"Stopping server bindings")
    for {
      _ <- stopHttp
      _ <- stopWs
    } yield {
      logger.info(s"Done stopping server bindings")
      ()
    }
  }
}
