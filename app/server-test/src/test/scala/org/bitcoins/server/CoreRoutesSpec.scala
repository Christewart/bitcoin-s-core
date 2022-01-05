package org.bitcoins.server

import akka.http.scaladsl.model.{ContentTypes, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.dlc.DLCTestUtil
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

class CoreRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  val coreRoutes = CoreRoutes()

  //https://test.oracle.suredbits.com/announcement/362ae482860fc93bac5cbcca3f1f0e49b3c94eac92224a008bd81ef81292f43a
  val numericAnnouncement = OracleAnnouncementTLV.fromHex(
    "fdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65"
  )

  "Core routes" should {
    "decode an accept message" in {
      val args = ujson.Arr(DLCTestUtil.acceptHex)
      val route =
        coreRoutes.handleCommand(ServerCommand("decodeaccept", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        val actualJson = ujson.read(responseAs[String])
        assert(actualJson == DLCTestUtil.expectedAccept)
      }
    }

    "decode a sign message" in {
      val args = ujson.Arr(DLCTestUtil.signHex)
      val route =
        coreRoutes.handleCommand(ServerCommand("decodesign", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        val actualJson = ujson.read(responseAs[String])
        assert(actualJson == DLCTestUtil.expectedSign)
      }
    }

    "decode an announcement" in {
      val responsesF: Future[Vector[RouteTestResult]] = Future.sequence {
        0.until(10000)
          .map { _ =>
            val f = Future {
              //println(s"Start with $i")
              val response = decodeAnnouncement()
              response
            }
            f.failed.foreach(err => println(s"err=$err"))
            f.map { r =>
              //println(s"Done with $i")
              r
            }
          }
          .toVector
      }

      val responses = Await.result(responsesF, 10.seconds)
      val result = responses.forall { r =>
        if (r.response.status != StatusCodes.OK) {
          println(s"status code=${r.response}")
        }
        r.response.status == StatusCodes.OK
      }
      assert(result)
    }
  }

  def decodeAnnouncement(): RouteTestResult = {
    val args = ujson.Arr(numericAnnouncement.hex)
    val route =
      coreRoutes.handleCommand(ServerCommand("decodeannouncement", args))

    Post() ~> route
  }
}
