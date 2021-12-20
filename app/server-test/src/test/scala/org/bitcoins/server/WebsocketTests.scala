package org.bitcoins.server

import akka.http.scaladsl.model.ws.WebSocketUpgradeResponse
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.ws.WalletNotification
import org.bitcoins.commons.jsonmodels.ws.WalletNotification.NewAddressNotification
import org.bitcoins.commons.serializers.WsPicklers
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.testkit.server.{
  BitcoinSServerMainBitcoindFixture,
  ServerWithBitcoind
}

import scala.concurrent.{Future, Promise}

class WebsocketTests extends BitcoinSServerMainBitcoindFixture {

  behavior of "Websocket Tests"

  val endSink: Sink[WalletNotification[_], Future[Seq[WalletNotification[_]]]] =
    Sink.seq[WalletNotification[_]]

  val sink: Sink[Message, Future[Seq[WalletNotification[_]]]] = Flow[Message]
    .map {
      case message: TextMessage.Strict =>
        //we should be able to parse the address message
        val text = message.text
        val notification: WalletNotification[_] = {
          try {
            upickle.default.read[WalletNotification[_]](text)(
              WsPicklers.walletNotificationPickler)
          } catch {
            case err: Throwable =>
              logger.error(s"Failed to parse notification", err)
              throw err
          }
        }
        logger.info(s"Notification=$notification")
        notification
      case msg =>
        logger.error(s"msg=$msg")
        fail(s"Unexpected msg type received in the sink, msg=$msg")
    }
    .toMat(endSink)(Keep.right)

  val req = WebSocketRequest("ws://localhost:19999/events")

  it must "receive updates when an address is generated" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(_, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
      //start the websocket

      val sourceKickOff = Source
        .single(TextMessage("kick off msg"))
        // Keeps the connection open
        .concatMat(Source.maybe[Message])(Keep.right)

      val f: Flow[
        Message,
        Message,
        (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])] = {
        Flow
          .fromSinkAndSourceMat(sink, sourceKickOff)(Keep.both)
          .log("@FLOW@")
      }

      val notificationsF: (
          Future[WebSocketUpgradeResponse],
          (Future[Seq[WalletNotification[_]]], Promise[Option[Message]])) = {
        Http()
          .singleWebSocketRequest(req, f)
      }

      val walletNotificationsF: Future[Seq[WalletNotification[_]]] =
        notificationsF._2._1

      val promise: Promise[Option[Message]] = notificationsF._2._2
      logger.info(s"Requesting new address for expectedAddrStr")
      val expectedAddressStr = ConsoleCli
        .exec(CliCommand.GetNewAddress(labelOpt = None), cliConfig)
        .get
      val expectedAddress = BitcoinAddress.fromString(expectedAddressStr)

      promise.success(None)
      logger.info(s"before notificationsF")
      for {
        notifications <- walletNotificationsF
        _ = logger.info(s"after notificationsF")
      } yield {
        assert(
          notifications.exists(_ == NewAddressNotification(expectedAddress)))
      }
  }

  /*  it must "receive updates when a transaction is broadcast" in {
    serverWithBitcoind =>
      val ServerWithBitcoind(bitcoind, server) = serverWithBitcoind
      val cliConfig = Config(rpcPortOpt = Some(server.conf.rpcPort))
      //start the websocket
      val notificationF: Future[WalletNotification[_]] = {
        Http()
          .webSocketClientFlow(req)
          .viaMat(flow)(Keep.right)
          .runWith(Source.empty, sink)
          ._2
      }

      val addressF = bitcoind.getNewAddress
      for {
        address <- addressF
        cmd = CliCommand.SendToAddress(destination = address,
                                       amount = Bitcoins.one,
                                       satoshisPerVirtualByte =
                                         Some(SatoshisPerVirtualByte.one),
                                       noBroadcast = false)
        balance = ConsoleCli.exec(CliCommand.GetBalance(false), cliConfig)
        txIdStr = ConsoleCli.exec(cmd, cliConfig)
        expectedTxId = DoubleSha256DigestBE.fromHex(txIdStr.get)
        notification <- notificationF
      } yield {
        notification match {
          case TxBroadcastNotification(tx) =>
            assert(tx.txIdBE == expectedTxId)
          case x =>
            fail(s"Expected tx broadcast notitfication, got=$x")
        }
      }
  }*/
}
