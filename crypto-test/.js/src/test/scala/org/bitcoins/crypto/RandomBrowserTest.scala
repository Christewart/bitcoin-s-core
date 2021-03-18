package org.bitcoins.crypto

class RandomBrowserTest extends BitcoinSCryptoTest {

  behavior of "RandomBrowser"

  it should "generate random bytes" in {
    forAll(BCryptoGen.secureRandomBrowserBytes) { bytes =>
      assert(bytes.nonEmpty)
      //make sure we don't have all zeros given to us
      assert(bytes.toArray.exists(_ != 0))
    }
  }

  it must "return two different outputs when calling it twice" in {
    forAll(BCryptoGen.secureRandomBrowserBytes(32),
           BCryptoGen.secureRandomBrowserBytes(32)) { case (bytes1, bytes2) =>
      assert(bytes1 != bytes2)
    }
  }

  it should "fail to generate zero bytes of randomness" in {
    assertThrows[RuntimeException] {
      BCryptoCryptoRuntime.randomBytes(0)
    }
  }

  it should "fail to genenerate a negative number of bytes of randomness" in {
    assertThrows[RuntimeException] {
      BCryptoCryptoRuntime.randomBytes(-1)
    }
  }
}
