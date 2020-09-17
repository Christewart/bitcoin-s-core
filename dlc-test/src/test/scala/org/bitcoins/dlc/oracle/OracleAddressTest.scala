package org.bitcoins.dlc.oracle

import java.util.Date

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.ln.{LnTag, LnTagPrefix}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.{ECPrivateKey, SchnorrNonce, SchnorrPublicKey}
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BitcoinSUnitTest}
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneOffset

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
    println(s"string=$string")
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

}
