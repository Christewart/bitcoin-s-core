package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  WitnessTxSigComponent,
  WitnessTxSigComponentRebuilt
}
import org.bitcoins.core.protocol.script.{
  ScriptSignature,
  TaprootKeyPath,
  TaprootScriptPath
}
import org.bitcoins.core.script.flag.ScriptVerifyTaproot
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.slf4j.LoggerFactory
import scodec.bits._

class TaprootTxTests extends BitcoinSUnitTest {

  behavior of "Taproot test cases"

  private val logger = LoggerFactory.getLogger(getClass)
  //these tests are from
  //https://raw.githubusercontent.com/bitcoin-core/qa-assets/main/unit_test_data/script_assets_test.json
  lazy val url = getClass.getResource("/script_assets_test.json")

  lazy val lines = {
    scala.io.Source.fromURL(url).getLines().mkString
  }

  lazy val testCases: Seq[TaprootTestCase] = {
    upickle.default.read[Seq[TaprootTestCase]](lines)
  }
  it must "parse a taproot test case" in {
    //https://github.com/bitcoin/bitcoin/blob/v22.0/test/functional/feature_taproot.py#L1112
    //https://github.com/bitcoin/bitcoin/blob/3820090bd619ac85ab35eff376c03136fe4a9f04/src/test/script_tests.cpp#L1673
    val first = testCases.head
    val expectedTxHex =
      "01000000018ac8b525460593c8d490a4d73d99126309191b84770b43ff226feb48d517b721c500000000482137f0011b0656000000000016001470b9b7ca06422cf6c7011977c96c1914374a8462e5010000"
    val expectedPrevOutHex =
      "5e6aae010000000022512045cad6b20c81a782892f064caeab47cad9c276a917bed28ac30435e343a82188"
    assert(first.flags.contains(ScriptVerifyTaproot))
    assert(first.tx.hex == expectedTxHex)
    assert(first.prevouts.map(_.hex) == Vector(expectedPrevOutHex))
    assert(first.index == 0)
    assert(first.success._1 == ScriptSignature.empty)

    val witBytes = Vector(
      hex"2c6347f19bd72e40ff0d3ffcb872973ead3100bd0dc39d2dc48d31bb039e0f281f24c963404922771ef28ec09ec6f3875dca076f8ebc0c59d99cfa3e0eafdf0483",
      hex"",
      hex"4ddb016458021de049979f9df14b08fadbdd01a3bfa1a34d0836c9ba703646dba7690bd1c651e117bf8372cbdb66c63839506ad469d70141c62b097f8672919837fbac1394af4111dbf0943b1fa66adc89a30d7ef3003e346db4956bcdc42aa2db92975ac5bbda74bcc9759c2f9ae4c470f7cad700261b95327b213a3be13a1c017794a0d03ca7785c023ce9dc01bd087ed707cad46e2e3380b9c20311ca1579469f6539183792f94ed81dfba076a1178f0552a34cb46a385b16911cb8f2d488d33c8ffb5878e8b94f057533674247fff4c1459ec17e9f51e84c1b86bc92aa3e99fa56f8c663c006b3f1a2ef3718f9f7d6cdd23c5e6bff93254a88b8cb92b1951008f4fd025d761ef6dd6875160550002d5d061b208680caf845ee8aeefd84d91b2334af649d1a977484a250be89bd578dd287e1dd5ac82826fcfc3e6b7abbde717a9fee43dca49db2dba31a67760424a18ab210d8df4d9ed10556dfc2705245b525dabc0fc5329d557ab56315b69f5aea26adfc4729e1329769d00db2a18e1cd78a9b4690c3196e6da7c17617b7398b7a10ad3bf578a9091a908cb67972d75afdd5da8c8cfb4cc002bc6bd2f5d9b46c8584e76276158b078829e987a4e3ff427e07730ce5a19b4932e1961b6c9a1139ddc5a51e7df848f64e50b96dfdb27563ab201cbc6403a1e2bfe3c9e7e7a5e8988bcd015515c82748a236fc87a0814b2390ed67ab207ca4fde194f2ce74ede0129eff73e13955c11c7d9ae59f4c76aea01c52a4e5f568ac",
      hex"c01cbc6403a1e2bfe3c9e7e7a5e8988bcd015515c82748a236fc87a0814b2390ed801fbd24116a58ab9033b015b1e889aa20dc3f119a49e6458cae2f8b6f042b5b"
    )
    val expectedWitness = TaprootScriptPath.fromStack(witBytes.reverse)
    println(expectedWitness.script)
    assert(first.success._2.get == expectedWitness)
    assert(first.`final`.contains(true))
  }

  it must "run the success test cases through the script interpreter" in {
    testCases.foreach { testCase =>
      logger.debug(
        s"=================testCase.comment=${testCase.comment}=================")
      logger.debug(s"txSigComponent=${testCase.successTxSigComponent}")
      logger.debug(
        s"scriptSig=${testCase.successTxSigComponent.scriptSignature}")
      logger.debug(s"spk=${testCase.successTxSigComponent.scriptPubKey}")
      logger.debug(
        s"======================================================================")

      withClue(testCase.comment) {
        val result = ScriptInterpreter.run(testCase.successProgram)
        assert(result == ScriptOk)
      }
    }
    // 0340cff7c056a75c7863186c4143078002b51be9ddeafdda49e340ffd189e4f05aa06e368597184dd738fe632d74435e7cf0ad950dea9b11589f8777c312a36607a20014b7bd36b0268ee872bb0c02976f7ed9586c2864ca0a00
  }

  it must "run the failure test cases through the script interpreter" in {
    testCases.foreach { testCase =>
      testCase.failureTxSigComponentsOpt match {
        case Some(failureTxSigComponent) =>
          logger.debug(
            s"=================testCase.comment=${testCase.comment}=================")
          logger.debug(s"txSigComponent=${failureTxSigComponent}")
          logger.debug(
            s"scriptSig=${testCase.failureTxSigComponentsOpt.map(_.scriptSignature)}")
          logger.debug(
            s"spk=${testCase.failureTxSigComponentsOpt.map(_.scriptPubKey)}")
          failureTxSigComponent match {
            case witTxSig: WitnessTxSigComponent =>
              logger.debug(s"wit=${witTxSig.witness}")
            case _: BaseTxSigComponent | _: WitnessTxSigComponentRebuilt =>
              ()
          }
          logger.debug(
            s"======================================================================")

          withClue(testCase.comment) {
            val result = ScriptInterpreter.run(testCase.failProgramOpt.get)
            assert(result != ScriptOk)
          }
        case None =>
          logger.info(s"Failed to parse failure test case=${testCase.comment}")
      }
    }
  }
}
