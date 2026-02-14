package org.bitcoins.crypto

import org.bitcoins.crypto.frost.FrostUtil
import scodec.bits.ByteVector

class FROSTTest extends BitcoinSCryptoTest {
  behavior of "FROST"

  it must "create a vss commitment and verify it" in {
    val seed = ByteVector.fill(31)(0x00) ++ ByteVector(0x01)
    val share1 = FrostUtil.deriveShare(seed, idx = 0)
    val id1 = 1L
    val commitment1 = FrostUtil.vssCommitment(seed, threshold = 1)

    assert(FrostUtil.vssVerify(share1, id = id1, commitments = commitment1))
    println(s"Done  with 1 !!!!")

    val share2 = FrostUtil.deriveShare(seed, idx = 1)
    val commitment2 = FrostUtil.vssCommitment(seed, threshold = 2)
    val id2 = 2L
    println(s"share2=$share2 id2=$id2 commitment2=$commitment2")

    assert(FrostUtil.vssVerify(share1, id = id1, commitments = commitment2))
    assert(FrostUtil.vssVerify(share2, id = id2, commitments = commitment2))

  }

  it must "create a simple 2/3 threshold and be example to produce valid signatures for all combinations of the threshold" in {
    val seed = ECPrivateKey.freshPrivateKey
    val threshold = 2
    val numShares = 3
    val result = FrostUtil.generateShares(seed,
                                          threshold = threshold,
                                          numShares = numShares)

    assert(result.ids.size == numShares)
    assert(result.commitments.size == threshold)
    assert(result.shares.size == numShares)

    result.shares.zip(result.ids).foreach { case (share, id) =>
      println(
        s"Verifying $id share=$share commitments.size=${result.commitments.size}")
      assert(FrostUtil.vssVerify(share, id, result.commitments))
      println(s"Verified $id share=$share")
    }
  }

}
