package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.dlc.models.{
  EnumSingleOracleInfo,
  NumericSingleOracleInfo,
  SingleContractInfo
}
import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  NumericEventDescriptorTLV
}
import org.bitcoins.server.routes._
import ujson._
import upickle.default._

case class DLCRoutes(dlcNode: DLCNodeApi)(implicit system: ActorSystem)
    extends ServerRoute
    with Logging {

  import system.dispatcher

  override def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getdlchostaddress", _) =>
      complete {
        dlcNode.getHostAddress.map { addr =>
          // need to do this otherwise string will contain <unresolved>
          val str = s"${addr.getHostName}:${addr.getPort}"
          Server.httpSuccess(str)
        }
      }

    case ServerCommand("acceptdlc", arr) =>
      withValidServerCommand(AcceptDLC.fromJsArr(arr)) {
        case AcceptDLC(offer, address, payoutAddressOpt, changeAddressOpt) =>
          complete {
            dlcNode
              .acceptDLCOffer(address,
                              offer,
                              payoutAddressOpt,
                              changeAddressOpt)
              .map { accept =>
                Server.httpSuccess(accept.toMessage.hex)
              }
          }
      }

    case ServerCommand("createcontractinfo", arr) =>
      withValidServerCommand(CreateContractInfo.fromJsArr(arr)) {
        case create: CreateContractInfo =>
          complete {
            val oracleInfo =
              create.announcementTLV.eventTLV.eventDescriptor match {
                case _: NumericEventDescriptorTLV =>
                  NumericSingleOracleInfo(create.announcementTLV)
                case _: EnumEventDescriptorV0TLV =>
                  EnumSingleOracleInfo(create.announcementTLV)
              }
            val contractInfo = SingleContractInfo(create.totalCollateral,
                                                  create.contractDescriptor,
                                                  oracleInfo)
            Server.httpSuccess(contractInfo.hex)
          }
      }

    case ServerCommand("listincomingoffers", _) =>
      complete {
        dlcNode.wallet.listIncomingDLCOffers().map { offers =>
          def toJson(io: IncomingDLCOfferDb): Value = {
            Obj(
              "hash" -> io.hash.hex,
              "receivedAt" -> io.receivedAt.getEpochSecond,
              "peer" -> io.peer.map(Str).getOrElse(Null),
              "message" -> io.message.map(Str).getOrElse(Null),
              "offerTLV" -> io.offerTLV.hex
            )
          }

          Server.httpSuccess(offers.map(toJson))
        }
      }

    case ServerCommand("registerincomingoffer", arr) =>
      withValidServerCommand(RegisterIncomingOffer.fromJsArr(arr)) { register =>
        complete {
          dlcNode.wallet
            .registerIncomingDLCOffer(register.offerTLV,
                                      register.peer,
                                      register.message)
            .map { hash =>
              Server.httpSuccess(hash.hex)
            }
        }
      }

    case ServerCommand("rejectincomingoffer", arr) =>
      withValidServerCommand(RejectIncomingOffer.fromJsArr(arr)) { reject =>
        complete {
          dlcNode.wallet.rejectIncomingDLCOffer(reject.hash).map { _ =>
            Server.httpSuccess(reject.hash.hex)
          }
        }
      }
  }
}
