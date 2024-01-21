package org.bitcoins.scripts
//
//import org.bitcoins.server.routes.BitcoinSRunner
//import org.bitcoins.server.util.BitcoinSAppScalaDaemon
//
//import scala.concurrent.Future
//
//case class ComputeSizeIncrease() extends BitcoinSRunner[Unit] {
//
//  override def start(): Future[Unit] = { ??? }
//
//  override def stop(): Future[Unit] = {
//    system
//      .terminate()
//      .map(_ => ())
//  }
//}
//
//object ComputeSizeIncrease extends BitcoinSAppScalaDaemon {
//
//  override val actorSystemName: String =
//    s"scan-bitcoind-${System.currentTimeMillis()}"
//
//  override val customFinalDirOpt = None
//
//  new ComputeSizeIncrease().run()
//}
