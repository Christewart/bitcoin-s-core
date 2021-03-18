package org.bitcoins.crypto

import org.scalacheck.Gen

class RandomTest extends BitcoinSCryptoTest {

  it should "generate random bytes" in {
    forAll(Gen.choose(0, 100)) { num =>
      val rnd = BCryptoCryptoRuntime.randomBytes(num)
      assert(rnd.size == num)
      assert(rnd.toArray.exists(_ != 0))
    }
  }

  it must "return two different outputs when calling it twice" in {
    forAll(BCryptoGen.secureRandomBytes(32), BCryptoGen.secureRandomBytes(32)) {
      case (bytes1, bytes2) =>
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
