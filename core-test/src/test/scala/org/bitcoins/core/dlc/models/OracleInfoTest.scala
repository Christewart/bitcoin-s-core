package org.bitcoins.core.dlc.models

import org.bitcoins.core.protocol.dlc.models.{
  NumericOracleSignaturesUnsorted,
  NumericSingleOracleInfo
}
import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementV0TLV,
  OracleAttestmentV0TLV,
  UnsignedNumericOutcome
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class OracleInfoTest extends BitcoinSUnitTest {

  behavior of "OracleInfo"

  it must "verify a numeric oracle signature with nonces out of order" in {
    // see: https://github.com/bitcoin-s/bitcoin-s/issues/4808
    // see: https://oracle.suredbits.com/announcement/10827916922327c9e13e28b1b0338a0cf5e4d067909d95a9c9aec11f7ccace8d
    val announcementHex =
      "fdd824fd02d455abcf3b585c9729e92e13c9762c4adc9c6062be721ae217a8aac0ff92a73e837231809bf6f6b75192d665b301c0f4011f247bdc00c8752e524d65baeb1c3deb04ba9838623f02c940d20d7b185d410178cff7990c7fcf19186c7f58c7c4b8defdd822fd026e0012218b59ec12cb8b221e00ef6d3b88a19650b3fb99ea3b6d0f9ef8c356741347c16319d4bfa31978500f32876c6adbc18bee655f35dc97fa4824cf0775981542b96e658c9c5d257576db17d454806b6bd366c26bedf1ae358e1715377449b23c39afed7b41274028982c274bd01b799b35db36a6b31c10211dc05fe3cd3b059099361ecb7be6d7351aa516bea56a6100b4c2635da049c6252f94a0e117055314793c550569820e6687fbd5a60e80d7e026bf1d6f6afffd0d0778da023727d5f5b7aae45e2b8848539fe3485bcd64b58556d2a60d10e74e1ec541d5871f012960a669415a50ed291b4d9e50cc695ff0458573acda2881aa00f50fbe48a59f5f518416022bd073bea49df9940cbeb97fcdd2fe13957bc410950d0c9e971d6a6da197e5090d9821785e7b756ec56dc059b65d2fd3fa8f02595cd292ec10c0ca680e36ea89a62deb71fce778e0a603ed345adf03c3bc55e3d85c211aea3a15aee67fabe1e3081736280e3b54037e8ab5211bf9e8e27b61656f597843dc7be6464705e341e2a6b2fa96df10fd569c62d04dd9515cf1cf452e76c4f5147c125e0b5a5a54af141f492423370c9b41bc9259e4d9cc35553252d71fcc0c37c6f025afb1b11503499fa5f6f7dae9378ec1f8f749836e23f40326209194ab2779b6069f74cb562d1fbd83a7c7147549d866c3c71e0dade3e1d74dee457cbd3fcac11be8e71ce6039b3a9b60e3a4c707d725e2e43a456c4cdfe272ac1a63ac0cde984d81469903d9420c2902de121e9104126c89ab589698208926360bf052b84a4cb6504f4da763355080fdd80a100002000642544355534400000000001213446572696269742d4254432d32395345503232"
    val attestmentHex =
      "fdd868fd04da13446572696269742d4254432d3239534550323204ba9838623f02c940d20d7b185d410178cff7990c7fcf19186c7f58c7c4b8de0012218b59ec12cb8b221e00ef6d3b88a19650b3fb99ea3b6d0f9ef8c356741347c123b77bca5ecc8bc79872429bb14482409ee956ea56a80cabe7294075616de2d16319d4bfa31978500f32876c6adbc18bee655f35dc97fa4824cf0775981542b9d7765298590d55adb9fcf54dfaa61b06aef5063fafa8346e9fbef42b9920bee16e658c9c5d257576db17d454806b6bd366c26bedf1ae358e1715377449b23c39e44d24059828578783a2ea1114fad0170eceff86616c36609adf5e7521ede13dafed7b41274028982c274bd01b799b35db36a6b31c10211dc05fe3cd3b059099e1e498897895c816cc26cd3e1a3abbef94cfcadff613164d9389710a91c24d66361ecb7be6d7351aa516bea56a6100b4c2635da049c6252f94a0e1170553147967b116f18fd90b2a3de05efebe72c6ca6c43ee34fb19a5ce378df21c23ef679d3c550569820e6687fbd5a60e80d7e026bf1d6f6afffd0d0778da023727d5f5b75bdd717e2394a0922bd74fca168135353e938641f952eb555a03834f856cebd1aae45e2b8848539fe3485bcd64b58556d2a60d10e74e1ec541d5871f012960a6f31f71a9d5ae4bb55c275b22d520ff09f9cea9bdce59b22a31a5f773d2456c0569415a50ed291b4d9e50cc695ff0458573acda2881aa00f50fbe48a59f5f5184cbeb7e6e08f7f2fcd66aeab0091984cba01a760e9231729f992de59368636a3216022bd073bea49df9940cbeb97fcdd2fe13957bc410950d0c9e971d6a6da19706ca2fde38303e9a3e989e09380a823db07f7750534efaf3d10fc790344894e3e5090d9821785e7b756ec56dc059b65d2fd3fa8f02595cd292ec10c0ca680e36c626d082d4b553e3d8aa699486b684c9806d6a1bac3c36eb98285378da8e2153ea89a62deb71fce778e0a603ed345adf03c3bc55e3d85c211aea3a15aee67fabc54964f25eb9e54810bfb6113615e9bb2c3049990523348bf12a16fe4d42dc45e1e3081736280e3b54037e8ab5211bf9e8e27b61656f597843dc7be6464705e3d86d2678929708ada05b6dd1f713a53859279a31ee54e98b4addd3442e2a84a741e2a6b2fa96df10fd569c62d04dd9515cf1cf452e76c4f5147c125e0b5a5a5402b02196261f970104775a92110037afd4f73ebc66ad9d5ad1bf6220546682bbaf141f492423370c9b41bc9259e4d9cc35553252d71fcc0c37c6f025afb1b11530555b88dfc79e1fe4c2e29833403b1e0b0d7fd8eef2141a7e14dfe112a6749103499fa5f6f7dae9378ec1f8f749836e23f40326209194ab2779b6069f74cb567f4a8d6dd0e043a1fb557115c8d6d9cdb7dfb953615c789a472a9e1569011ff22d1fbd83a7c7147549d866c3c71e0dade3e1d74dee457cbd3fcac11be8e71ce6ea7c589ab40ec10bb9e5937a30fe460baae0ea654d68c8b14eb9c26c3c91ef45039b3a9b60e3a4c707d725e2e43a456c4cdfe272ac1a63ac0cde984d81469903fc5147237e764a865b9829271fc7f1d69f7d4b5564ac936b7def6afaf6bf7ae7d9420c2902de121e9104126c89ab589698208926360bf052b84a4cb6504f4da73524a82b6155921578c6085f45a40c2b7e3365e2370b259b6de92fc36a9a5d8a013001300130013101300130013101300131013101300131013101310131013001300131"
    val announcement = OracleAnnouncementV0TLV.fromHex(announcementHex)
    val attestment = OracleAttestmentV0TLV.fromHex(attestmentHex)
    val numericOracleInfo = NumericSingleOracleInfo(announcement)

    val digits = Vector(0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1)
    val outcome = UnsignedNumericOutcome(digits)
    val oracleSigs =
      NumericOracleSignaturesUnsorted(numericOracleInfo,
                                      attestment.unsortedSignatures)
    assert(numericOracleInfo.verifySigs(outcome, oracleSigs))
  }
}
