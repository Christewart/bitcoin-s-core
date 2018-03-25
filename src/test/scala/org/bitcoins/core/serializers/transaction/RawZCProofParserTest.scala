package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.protocol.transaction.ZCProof
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by str4d on 3/26/18.
 */
class RawZCProofParserTest extends FlatSpec with MustMatchers {

  val encode = BitcoinSUtil.encodeHex(_: Seq[Byte])
  val xencode = (p: Tuple2[Boolean, Seq[Byte]]) => (p._1, encode(p._2))

  val rawProof1 = "0317b334db90c8749058f2404275ab8f735fd4f0e2eb8a878bb500ec6a5c26ca43020205ddf30a009ce6a21c54fad88e9dec07d6d3630db6ee3e12b04940f6a0fba00b02747fd2af896188701bd2dec8b3a3a43362b9a1603717f30f70e7f63c59fd90dcdab674a6f3cf45d989c5ebdc8457dc2af27d05fa920e9553c80b5fdbb28573021ae476f72a3398f9d59addf5c2e93358adac2854748d8ebc0df731e23bc63c17032c41baccadfabc31b767413e394c2cc5210e05bb0fb43dbb75a8aed831c3fd3f0214bb069c6576ae30157321be13efe604830d6d9d530470196e8ec8c75a5826e9030d2ce45777d8049ef7ec5fbf45a2372dd17f470c3757bbd4ac751a7dc74b376403059ab875b8575ac473e8aed90583e6167d3592cbf572da758d8643ef22bda7d2"
  "RawZCProofTest" must "read a serialized Sprout proof" in {
    val proof : ZCProof = RawZCProofParser.read(rawProof1)
    xencode(proof.gA) must be ((true, "17b334db90c8749058f2404275ab8f735fd4f0e2eb8a878bb500ec6a5c26ca43"))
    xencode(proof.gAPrime) must be ((false, "0205ddf30a009ce6a21c54fad88e9dec07d6d3630db6ee3e12b04940f6a0fba0"))
    xencode(proof.gB) must be ((true, "02747fd2af896188701bd2dec8b3a3a43362b9a1603717f30f70e7f63c59fd90dcdab674a6f3cf45d989c5ebdc8457dc2af27d05fa920e9553c80b5fdbb28573"))
    xencode(proof.gBPrime) must be ((false, "1ae476f72a3398f9d59addf5c2e93358adac2854748d8ebc0df731e23bc63c17"))
    xencode(proof.gC) must be ((true, "2c41baccadfabc31b767413e394c2cc5210e05bb0fb43dbb75a8aed831c3fd3f"))
    xencode(proof.gCPrime) must be ((false, "14bb069c6576ae30157321be13efe604830d6d9d530470196e8ec8c75a5826e9"))
    xencode(proof.gK) must be ((true, "0d2ce45777d8049ef7ec5fbf45a2372dd17f470c3757bbd4ac751a7dc74b3764"))
    xencode(proof.gH) must be ((true, "059ab875b8575ac473e8aed90583e6167d3592cbf572da758d8643ef22bda7d2"))
  }

  val rawProof2 = "020d33cf689264dfbe12e42806e8597b9834ef127c4723ac06e96645731e486438031e29277c1bba25160ed46433e20ed031cbf3c593119171ba79e45a5e3d88e11c0a052150e32a281f231cb71fb408ef2b5757413c6d5c83e8b13aa915053a0689678b87d60bd814b5a985ddc0452e12964bd42534d355c2bd3ed645b79a5c2d171c022760b09bebeded7c95ca8046d64eb1a06ee52fc00e5183fa37c2b9a600bb679f0221422fdb7f491fc51adc33820a5ad4f6c8a01f8f75b5e9a453c7aa16a108a68302105f99acfb406c86412b5c6bdf1a9a86538c6e92327ac11c853d28b184708aaf021b416342c222c1f6b3bcd91dfcf70acee747e0b7f8979b83d8cd539d4743ebec0328c3d7cf3a8c009ae97139ffe2ff65d4e4cfe9c69bf385f8c380656286844477"
  it must "read another serialized Sprout proof" in {
    val proof : ZCProof = RawZCProofParser.read(rawProof2)
    xencode(proof.gA) must be ((false, "0d33cf689264dfbe12e42806e8597b9834ef127c4723ac06e96645731e486438"))
    xencode(proof.gAPrime) must be ((true, "1e29277c1bba25160ed46433e20ed031cbf3c593119171ba79e45a5e3d88e11c"))
    xencode(proof.gB) must be ((false, "052150e32a281f231cb71fb408ef2b5757413c6d5c83e8b13aa915053a0689678b87d60bd814b5a985ddc0452e12964bd42534d355c2bd3ed645b79a5c2d171c"))
    xencode(proof.gBPrime) must be ((false, "2760b09bebeded7c95ca8046d64eb1a06ee52fc00e5183fa37c2b9a600bb679f"))
    xencode(proof.gC) must be ((false, "21422fdb7f491fc51adc33820a5ad4f6c8a01f8f75b5e9a453c7aa16a108a683"))
    xencode(proof.gCPrime) must be ((false, "105f99acfb406c86412b5c6bdf1a9a86538c6e92327ac11c853d28b184708aaf"))
    xencode(proof.gK) must be ((false, "1b416342c222c1f6b3bcd91dfcf70acee747e0b7f8979b83d8cd539d4743ebec"))
    xencode(proof.gH) must be ((true, "28c3d7cf3a8c009ae97139ffe2ff65d4e4cfe9c69bf385f8c380656286844477"))
  }

  it must "serialize a Sprout proof" in {
    val proof = RawZCProofParser.read(rawProof1)
    encode(RawZCProofParser.write(proof)) must be (rawProof1)
  }

  val invalidG1 = "1317b334db90c8749058f2404275ab8f735fd4f0e2eb8a878bb500ec6a5c26ca43020205ddf30a009ce6a21c54fad88e9dec07d6d3630db6ee3e12b04940f6a0fba00b02747fd2af896188701bd2dec8b3a3a43362b9a1603717f30f70e7f63c59fd90dcdab674a6f3cf45d989c5ebdc8457dc2af27d05fa920e9553c80b5fdbb28573021ae476f72a3398f9d59addf5c2e93358adac2854748d8ebc0df731e23bc63c17032c41baccadfabc31b767413e394c2cc5210e05bb0fb43dbb75a8aed831c3fd3f0214bb069c6576ae30157321be13efe604830d6d9d530470196e8ec8c75a5826e9030d2ce45777d8049ef7ec5fbf45a2372dd17f470c3757bbd4ac751a7dc74b376403059ab875b8575ac473e8aed90583e6167d3592cbf572da758d8643ef22bda7d2"
  it must "throw an error on invalid G1 encoding" in {
    val thrown = the [IllegalArgumentException] thrownBy RawZCProofParser.read(invalidG1)
    thrown.getMessage must be ("requirement failed: lead byte of G1 point not recognized")
  }

  val invalidG2 = "0317b334db90c8749058f2404275ab8f735fd4f0e2eb8a878bb500ec6a5c26ca43020205ddf30a009ce6a21c54fad88e9dec07d6d3630db6ee3e12b04940f6a0fba01b02747fd2af896188701bd2dec8b3a3a43362b9a1603717f30f70e7f63c59fd90dcdab674a6f3cf45d989c5ebdc8457dc2af27d05fa920e9553c80b5fdbb28573021ae476f72a3398f9d59addf5c2e93358adac2854748d8ebc0df731e23bc63c17032c41baccadfabc31b767413e394c2cc5210e05bb0fb43dbb75a8aed831c3fd3f0214bb069c6576ae30157321be13efe604830d6d9d530470196e8ec8c75a5826e9030d2ce45777d8049ef7ec5fbf45a2372dd17f470c3757bbd4ac751a7dc74b376403059ab875b8575ac473e8aed90583e6167d3592cbf572da758d8643ef22bda7d2"
  it must "throw an error on invalid G2 encoding" in {
    val thrown = the [IllegalArgumentException] thrownBy RawZCProofParser.read(invalidG2)
    thrown.getMessage must be ("requirement failed: lead byte of G2 point not recognized")
  }
}