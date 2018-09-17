package org.bitcoins.core.protocol.ln

import org.scalatest.{ FlatSpec, MustMatchers }

class LnCurrencyUnitTest extends FlatSpec with MustMatchers {
  it must "serialize MilliSatoshis to string" in {
    val milliSatoshis = MilliSatoshis(1000)
    milliSatoshis.toEncodedString must be("1000m")
  }

  it must "serialize MicroSatoshis to string" in {
    val microSatoshis = MicroSatoshis(1000)
    microSatoshis.toEncodedString must be("1000u")
  }

  it must "serialize NanoSatoshis to string" in {
    val nanoSatoshis = NanoSatoshis(1000)
    nanoSatoshis.toEncodedString must be("1000n")
  }

  it must "serialize PicoSatoshis to string" in {
    val picoSatoshis = PicoSatoshis(1000)
    picoSatoshis.toEncodedString must be("1000p")
  }

  it must "deserialize MilliSatoshis from string" in {
    val input = "1000m"
    LnCurrencyUnits.fromEncodedString(input).get must be(MilliSatoshis(1000))
  }

  it must "deserialize MicroSatoshis from string" in {
    val input = "1000u"
    LnCurrencyUnits.fromEncodedString(input).get must be(MicroSatoshis(1000))
  }

  it must "deserialize NanoSatoshis from string" in {
    val input = "1000n"
    LnCurrencyUnits.fromEncodedString(input).get must be(NanoSatoshis(1000))
  }

  it must "deserialize PicoSatoshis from string" in {
    val input = "1000p"
    LnCurrencyUnits.fromEncodedString(input).get must be(PicoSatoshis(1000))
  }

  it must "fail to deserialize an invalid amount" in {
    val input = "10000000000000000m"
    LnCurrencyUnits.fromEncodedString(input).isFailure must be(true)
  }

  it must "fail to deserialize an invalid number" in {
    val input = "10z00m"
    LnCurrencyUnits.fromEncodedString(input).isFailure must be(true)
  }

  it must "fail to deserialize an invalid currency denomination" in {
    val input = "1000z"
    LnCurrencyUnits.fromEncodedString(input).isFailure must be(true)
  }

  it must "have the correct maximum and minimum number representation for MilliSatoshis" in {
    MilliSatoshis.max must be(MilliSatoshis(9223372036L))
    MilliSatoshis.min must be(MilliSatoshis(-9223372036L))
  }

  it must "have the correct maximum and minimum number representation for MicroSatoshis" in {
    MicroSatoshis.max must be(MicroSatoshis(9223372036854L))
    MicroSatoshis.min must be(MicroSatoshis(-9223372036854L))
  }

  it must "have the correct maximum and minimum number representation for NanoSatoshis" in {
    NanoSatoshis.max must be(NanoSatoshis(9223372036854775L))
    NanoSatoshis.min must be(NanoSatoshis(-9223372036854775L))
  }

  it must "have the correct maximum and minimum number representation for PicoSatoshis" in {
    PicoSatoshis.max must be(PicoSatoshis(9223372036854775807L))
    PicoSatoshis.min must be(PicoSatoshis(-9223372036854775808L))
  }

  it must "fail to create a MilliBitcoin outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      MilliSatoshis(LnPolicy.maxMilliSatoshis + 1)
    }
  }

  it must "fail to create a MicroBitcoin outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      MicroSatoshis(LnPolicy.maxMicroSatoshis + 1)
    }
  }

  it must "fail to create a NanoBitcoin outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      NanoSatoshis(LnPolicy.maxNanoSatoshis + 1)
    }
  }

  it must "fail to create a PicoBitcion outside of the maximum range" in {
    intercept[IllegalArgumentException] {
      PicoSatoshis(LnPolicy.maxPicoSatoshis + 1)
    }
  }

  it must "fail to create a MilliBitcoin outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      MilliSatoshis(LnPolicy.minMilliSatoshis - 1)
    }
  }

  it must "fail to create a MicroBitcoin outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      MicroSatoshis(LnPolicy.minMicroSatoshis - 1)
    }
  }

  it must "fail to create a NanoBitcoin outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      NanoSatoshis(LnPolicy.minNanoSatoshis - 1)
    }
  }

  it must "fail to create a PicoBitcion outside of the minimum range" in {
    intercept[IllegalArgumentException] {
      PicoSatoshis(LnPolicy.minPicoSatoshis - 1)
    }
  }

  it must "have the correct representation for 0" in {
    MilliSatoshis.zero must be(MilliSatoshis(0))
    MicroSatoshis.zero must be(MicroSatoshis(0))
    NanoSatoshis.zero must be(NanoSatoshis(0))
    PicoSatoshis.zero must be(PicoSatoshis(0))
    LnCurrencyUnits.zero must be(PicoSatoshis(0))
  }

  it must "have the correct representation for 1" in {
    MilliSatoshis.one must be(MilliSatoshis(1))
    MicroSatoshis.one must be(MicroSatoshis(1))
    NanoSatoshis.one must be(NanoSatoshis(1))
    PicoSatoshis.one must be(PicoSatoshis(1))
    LnCurrencyUnits.oneMilliBTC must be(MilliSatoshis(1))
    LnCurrencyUnits.oneMicroBTC must be(MicroSatoshis(1))
    LnCurrencyUnits.oneNanoBTC must be(NanoSatoshis(1))
    LnCurrencyUnits.onePicoBTC must be(PicoSatoshis(1))
  }
}