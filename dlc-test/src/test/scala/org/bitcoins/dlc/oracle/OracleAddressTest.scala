package org.bitcoins.dlc.oracle

import java.util.Date

import org.bitcoins.core.number.{Int32, UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.ln.{LnTag, LnTagPrefix}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.{
  ECPrivateKey,
  FieldElement,
  SchnorrDigitalSignature,
  SchnorrNonce,
  SchnorrPublicKey
}
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BitcoinSUnitTest}
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset

import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  OracleAnnouncementV0TLV,
  OracleEventV0TLV,
  RangeEventDescriptorV0TLV
}

class OracleAddressTest extends BitcoinSUnitTest {

  behavior of "OracleAddressTest"

  val privKey = ECPrivateKey.fromHex(
    "56a70a2742d9e7835e2258fac7d81cf78c376e51be24f80504d8e10e585b6ca2")
  val pubKey = privKey.schnorrPublicKey

  val pubKeyTag = LnTag.DLCPubKey(pubKey)

  val nonce = SchnorrNonce.fromHex(
    "bf9582bca72dac2304cf228bc0bee0d6d73ebab0151a89aa9b9bbc67a72acf9c")

  val nonceTag = LnTag.DLCNonce(nonce)
  val expiration = new Date(2020, 11, 3).toInstant.toEpochMilli / 1000

  val localDate: LocalDate = LocalDate.parse("2020-11-03")
  val localDateTime: LocalDateTime = localDate.atStartOfDay()
  val instant: Instant = localDateTime.toInstant(ZoneOffset.UTC)
  val u32 = UInt32(instant.toEpochMilli / 1000)
  val u64 = UInt64(instant.toEpochMilli / 1000)
  val maturation = LnTag.DLCMaturation(seconds = u64)

  val outcomes = Vector("sunny", "cloudy", "rainy")
  val hrp = OracleHumanReadablePart.hrp

  val oracleAddress =
    OracleAddress(hrp = hrp,
                  publicKey = pubKey,
                  nonce = nonce,
                  maturation = maturation,
                  outcomes = outcomes)
  it must "read/write an address to a string" in {

    val string = oracleAddress.toString
    val oracleAddress2 = OracleAddress.fromString(string)
    assert(
      string == "oracle1gp50v8mlfermwpqyc5hpm29733s20wxr0kv8czj75vuecp3ahp07jtsep5h72c90989kkzxpx0y29up0hq6mtnaw4sz5dgn25mnw7x0fe2e7wqmqdqqqqqqzl5zwcq0qgwd6kumne0q2vdkx7aty0y0qgwfskjmnege9q2x")
    assert(string == oracleAddress2.toString)
    assert(oracleAddress.hrp == oracleAddress2.hrp)

    assert(oracleAddress.publicKey == pubKeyTag)
    assert(
      oracleAddress.publicKey.toString == "gp50v8mlfermwpqyc5hpm29733s20wxr0kv8czj75vuecp3ahp07jts")
    assert(oracleAddress.publicKey == oracleAddress2.publicKey)

    assert(oracleAddress.nonce == nonceTag)
    assert(
      oracleAddress.nonce.toString == "ep5h72c90989kkzxpx0y29up0hq6mtnaw4sz5dgn25mnw7x0fe2e7wq")
    assert(oracleAddress.nonce == oracleAddress2.nonce)

    assert(oracleAddress.outcomes.length == 3)
    assert(oracleAddress.outcomes == oracleAddress2.outcomes)
    assert(oracleAddress.outcomes(0).toString == "0qgwd6kumne")
    assert(oracleAddress.outcomes(1).toString == "0q2vdkx7aty0y")
    assert(oracleAddress.outcomes(2).toString == "0qgwfskjmne")

    assert(oracleAddress.maturation.seconds == u64)
    assert(oracleAddress.maturation.toString == "mqdqqqqqqzl5zwcq")

    assert(oracleAddress.checksum == oracleAddress2.checksum)
    assert(Bech32.encode5bitToString(oracleAddress.checksum) == "ge9q2x")
    assert(oracleAddress == oracleAddress2)

  }

  val eventDescriptor = EnumEventDescriptorV0TLV(outcomes)
  val uri: String = ""

  val oracleEnumEventTLV: OracleEventV0TLV =
    OracleEventV0TLV(publicKey = pubKey,
                     nonce = nonce,
                     eventMaturityEpoch = u32,
                     eventDescriptor = eventDescriptor,
                     eventURI = uri)

  val signature: SchnorrDigitalSignature =
    SchnorrDigitalSignature(nonce, FieldElement(0))

  val announcement: OracleAnnouncementV0TLV =
    OracleAnnouncementV0TLV(signature, oracleEnumEventTLV)

  it must "read/write an oracle address based on a enum event tlv" in {
    val address = OracleAddressTlv(hrp, announcement)
    val expected =
      "oracle1lhvzff9ljkptefed4s3sfnez30qtacxk6ult4vq4r2y64xumh3n6w2k0nsqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqplwcyfs8kral5u3ahqszv2tsa4zlgcc98hrphmxrupf02xwvuqc7mshlf9aljkptefed4s3sfnez30qtacxk6ult4vq4r2y64xumh3n6w2k0n306p8vqlhvqvxqqqvqq2um4deh8jqqxvdkx7aty0yqq2unpd9h8jc2wt5l"
    assert(address.toString == expected)

    val oracleAddress2 = OracleAddressTlv.fromString(address.toString)

    assert(oracleAddress2 == address)
  }

  val rangeDescriptor: RangeEventDescriptorV0TLV = {
    RangeEventDescriptorV0TLV(Int32.zero, Int32(10), UInt16.one)
  }

  val oracleRangeEventTLV: OracleEventV0TLV = {
    oracleEnumEventTLV.copy(eventDescriptor = rangeDescriptor)
  }

  val rangeAnnouncement = announcement.copy(eventTLV = oracleRangeEventTLV)
  it must "read/write an oracle address based on a range event tlv" in {
    val address =
      OracleAddressTlv(hrp = hrp, oracleAnnouncementTLV = rangeAnnouncement)

    assert(
      address.toString == "oracle1lhvzf94ljkptefed4s3sfnez30qtacxk6ult4vq4r2y64xumh3n6w2k0nsqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqplwcyff8kral5u3ahqszv2tsa4zlgcc98hrphmxrupf02xwvuqc7mshlf9aljkptefed4s3sfnez30qtacxk6ult4vq4r2y64xumh3n6w2k0n306p8vqlhvqszsqqqqqqqqqqq9qqqghgle3x")

    val oracleAddress2 = OracleAddressTlv.fromString(address.toString)

    assert(oracleAddress2 == address)
  }
}
