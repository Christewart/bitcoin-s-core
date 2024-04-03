package org.bitcoins.core.protocol.script.descriptor

import org.bitcoins.core.protocol.script.{
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WSHWitnessSPKV0,
  ScriptPubKey
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalatest.Assertion

import scala.annotation.tailrec

class DescriptorTest extends BitcoinSUnitTest {

  behavior of "OutputDescriptor"

  it must "parse valid descriptors in BIP382" in {
    val str0 = "wpkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)"
    val expected0 = "00149a1c78a507689f6f54b847ad1cef1e614ee23f1e"
    runTest(str0, expected0)

    val str1 =
      "wpkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    val expected1 = "00149a1c78a507689f6f54b847ad1cef1e614ee23f1e"
    runTest(str1, expected1)

    val str2 =
      "wpkh([ffffffff/13']xprv9vHkqa6EV4sPZHYqZznhT2NPtPCjKuDKGY38FBWLvgaDx45zo9WQRUT3dKYnjwih2yJD9mkrocEZXo1ex8G81dwSM1fwqWpWkeS3v86pgKt/1/2/0)"
    val expected2 = "0014326b2249e3a25d5dc60935f044ee835d090ba859"
    runTest(str2, expected2)

    val str3 =
      "wpkh([ffffffff/13']xpub69H7F5d8KSRgmmdJg2KhpAK8SR3DjMwAdkxj3ZuxV27CprR9LgpeyGmXUbC6wb7ERfvrnKZjXoUmmDznezpbZb7ap6r1D3tgFxHmwMkQTPH/1/2/*)"
    val expected3 = Vector("0014326b2249e3a25d5dc60935f044ee835d090ba859",
                           "0014af0bd98abc2f2cae66e36896a39ffe2d32984fb7",
                           "00141fa798efd1cbf95cebf912c031b8a4a6e9fb9f27")
    runDerivationTest(str3, expected3)

    val str4 =
      "sh(wpkh(xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi/10/20/30/40/*'))"
    val expected4 = Vector("a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87",
                           "a914bed59fc0024fae941d6e20a3b44a109ae740129287",
                           "a9148483aa1116eb9c05c482a72bada4b1db24af654387")
    runDerivationTest(str4, expected4)

    val str5 =
      "sh(wpkh(xprv9s21ZrQH143K3QTDL4LXw2F7HEK3wJUD2nW2nRk4stbPy6cq3jPPqjiChkVvvNKmPGJxWUtg6LnF5kejMRNNU3TGtRBeJgk33yuGBxrMPHi/10/20/30/40/*'))"
    val expected5 = Vector("a9149a4d9901d6af519b2a23d4a2f51650fcba87ce7b87",
                           "a914bed59fc0024fae941d6e20a3b44a109ae740129287",
                           "a9148483aa1116eb9c05c482a72bada4b1db24af654387")
    runDerivationTest(str5, expected5)

    val str6 = "wsh(pkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected6 =
      "0020338e023079b91c58571b20e602d7805fb808c22473cbc391a41b1bd3a192e75b"
    runTest(str6, expected6)

    val str7 =
      "wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected7 =
      "0020338e023079b91c58571b20e602d7805fb808c22473cbc391a41b1bd3a192e75b"
    runTest(str7, expected7)

    val str8 = "wsh(pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected8 =
      "00202e271faa2325c199d25d22e1ead982e45b64eeb4f31e73dbdf41bd4b5fec23fa"
    runTest(str8, expected8)

    val str9 =
      "wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected9 =
      "0020338e023079b91c58571b20e602d7805fb808c22473cbc391a41b1bd3a192e75b"
    runTest(str9, expected9)

    val str10 = "wsh(pk(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1))"
    val expected10 =
      "00202e271faa2325c199d25d22e1ead982e45b64eeb4f31e73dbdf41bd4b5fec23fa"
    runTest(str10, expected10)

    val str11 =
      "wsh(pk(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    val expected11 =
      "00202e271faa2325c199d25d22e1ead982e45b64eeb4f31e73dbdf41bd4b5fec23fa"
    runTest(str11, expected11)

    val str12 =
      "sh(wsh(pkh(L4rK1yDtCWekvXuE6oXD9jCYfFNV2cWRpVuPLBcCU2z8TrisoyY1)))"
    val expected12 = "a914b61b92e2ca21bac1e72a3ab859a742982bea960a87"
    runTest(str12, expected12)

    val str13 =
      "sh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    val expected13 = "a914b61b92e2ca21bac1e72a3ab859a742982bea960a87"
    runTest(str13, expected13)

  }

  it must "fail to parse invalid test vectors from BIP382" in {
    val str0 = "wpkh(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss)"
    runFailTest(str0)
    val str1 = "sh(wpkh(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss))"
    runFailTest(str1)
    val str2 =
      "wpkh(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235)"
    runFailTest(str2)
    val str3 =
      "sh(wpkh(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235))"
    runFailTest(str3)
    val str4 = "wsh(pk(5KYZdUEo39z3FPrtuX2QbbwGnNP5zTd7yyr2SC1j299sBCnWjss))"
    runFailTest(str4)
    val str5 =
      "wsh(pk(04a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd5b8dec5235a0fa8722476c7709c02559e3aa73aa03918ba2d492eea75abea235))"
    runFailTest(str5)
    val str6 =
      "wsh(wpkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))"
    runFailTest(str6)
    val str7 =
      "wsh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    runFailTest(str7)
    val str8 =
      "sh(wsh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd))))"
    runFailTest(str8)
    val str9 =
      "wpkh(wsh(pkh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)))"
    runFailTest(str9)
    val str10 =
      "wsh(03a34b99f22c790c4e36b2b3c2c35a36db06226e41c692fc82b8b56ac1c540c5bd)"
    runFailTest(str10)
  }

  def runTest(descriptor: String, expectedSPK: String): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    val expected = ScriptPubKey.fromAsmHex(expectedSPK)
    assert(desc.scriptPubKey == expected)
    assert(desc.toString == descriptor)
  }

  @tailrec
  private def parseExtKeyExpression(
      expression: ScriptExpression): ExtKeyExpression = {
    expression match {
      case x: KeyExpressionScriptExpression =>
        x.source.asInstanceOf[ExtKeyExpression]
      case x: NestedScriptExpression =>
        parseExtKeyExpression(x.source)
      case x: RawScriptExpression =>
        sys.error(
          s"RawScriptExpression cannot be used in runDerivationTest(), got=$x")
    }
  }

  def runDerivationTest(
      descriptor: String,
      expectedSPKs: Vector[String]): Assertion = {
    val desc = ScriptDescriptor.fromString(descriptor)
    val extKeyDesc = parseExtKeyExpression(desc.expression)
    expectedSPKs.zipWithIndex.foreach { case (s, idx) =>
      val expected = ScriptPubKey.fromAsmHex(s)
      val derivedKey = extKeyDesc match {
        case xprv: XprvKeyExpression => xprv.deriveChild(idx).publicKey
        case xpub: XpubKeyExpression => xpub.deriveChild(idx)
      }

      val p2wpkh = P2WPKHWitnessSPKV0(derivedKey)
      val spk = desc.expression.descriptorType match {
        case DescriptorType.WPKH => p2wpkh
        case DescriptorType.SH   => P2SHScriptPubKey(p2wpkh)
        case DescriptorType.WSH  => P2WSHWitnessSPKV0(p2wpkh)
        case x                   => sys.error(s"Not supported by BIP382, got=$x")
      }
      assert(spk == expected)
    }
    succeed
  }

  private def runFailTest(str: String): Assertion = {
    assertThrows[RuntimeException] {
      KeyExpression.fromString(str)
    }
  }
}
