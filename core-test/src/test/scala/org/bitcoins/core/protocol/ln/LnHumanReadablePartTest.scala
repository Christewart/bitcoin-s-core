package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config.{ MainNet, RegTest, TestNet3 }
import org.bitcoins.core.protocol.ln.LnParams._
import org.scalatest.{ FlatSpec, MustMatchers }

import scala.util.Try

class LnHumanReadablePartTest extends FlatSpec with MustMatchers {
  val mBtc = MilliSatoshis(1000)
  val mBtcOpt = Some(mBtc)

  behavior of "LnHumanReadablePart"

  it must "match the correct hrp with the correct network" in {
    LnHumanReadablePart(MainNet).get must be(LnHumanReadablePart(LnBitcoinMainNet))
    LnHumanReadablePart(TestNet3).get must be(LnHumanReadablePart(LnBitcoinTestNet))
    LnHumanReadablePart(RegTest).get must be(LnHumanReadablePart(LnBitcoinRegTest))

    LnHumanReadablePart(MainNet, mBtc).get must be(LnHumanReadablePart(LnBitcoinMainNet, mBtcOpt))
    LnHumanReadablePart(TestNet3, mBtc).get must be(LnHumanReadablePart(LnBitcoinTestNet, mBtcOpt))
    LnHumanReadablePart(RegTest, mBtc).get must be(LnHumanReadablePart(LnBitcoinRegTest, mBtcOpt))
  }

  it must "correctly serialize the hrp to string" in {
    LnHumanReadablePart(LnBitcoinMainNet, mBtcOpt).toString must be("lnbc1000m")
    LnHumanReadablePart(LnBitcoinTestNet, mBtcOpt).toString must be("lntb1000m")
    LnHumanReadablePart(LnBitcoinRegTest, mBtcOpt).toString must be("lnbcrt1000m")

    LnHumanReadablePart(LnBitcoinMainNet).toString must be("lnbc")
    LnHumanReadablePart(LnBitcoinTestNet).toString must be("lntb")
    LnHumanReadablePart(LnBitcoinRegTest).toString must be("lnbcrt")
  }

  it must "fail to create hrp from invalid amount" in {
    val tooBig = Some(MilliSatoshis(LnPolicy.maxAmountMSat.toBigInt + 1))
    val zero = Some(LnCurrencyUnits.zero)
    val tooSmall = Some(MilliSatoshis(-1))

    Try(LnHumanReadablePart(LnBitcoinMainNet, tooBig)).isFailure must be(true)
    Try(LnHumanReadablePart(LnBitcoinMainNet, zero)).isFailure must be(true)
    Try(LnHumanReadablePart(LnBitcoinMainNet, tooSmall)).isFailure must be(true)

  }

  it must "deserialize hrp from string" in {

    LnHumanReadablePart.fromString("lnbc").get must be(LnHumanReadablePart(LnBitcoinMainNet))
    LnHumanReadablePart.fromString("lntb").get must be(LnHumanReadablePart(LnBitcoinTestNet))
    LnHumanReadablePart.fromString("lnbcrt").get must be(LnHumanReadablePart(LnBitcoinRegTest))

    LnHumanReadablePart.fromString("lnbc1000m").get must be(LnHumanReadablePart(LnBitcoinMainNet, mBtcOpt))
    LnHumanReadablePart.fromString("lntb1000m").get must be(LnHumanReadablePart(LnBitcoinTestNet, mBtcOpt))
    LnHumanReadablePart.fromString("lnbcrt1000m").get must be(LnHumanReadablePart(LnBitcoinRegTest, mBtcOpt))
  }

  it must "fail to deserialize hrp from invalid string" in {
    LnHumanReadablePart.fromString("invalid").isFailure must be(true)
    LnHumanReadablePart.fromString("lnbc9000").isFailure must be(true)
    LnHumanReadablePart.fromString("lnbc90z0m").isFailure must be(true)

  }
}
