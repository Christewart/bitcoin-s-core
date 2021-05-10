package org.bitcoins.dlc.wallet

import org.bitcoins.core.protocol.tlv.ContractInfoV0TLV
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import java.nio.file.{Files, Paths}

class ComplexContractTest extends BitcoinSUnitTest {

  behavior of "ComplextContract"

  it must "verify a complex contract info" in {
    val fileName = "/2of3cfdnumeric18digit.txt"
    val f = getClass.getResource(fileName).toURI
    val x = Files.readAllBytes(Paths.get(f))
    val bytes = ByteVector(x)
    val contractInfo = ContractInfoV0TLV.fromBytes(bytes)
    println(s"contractInfo=$contractInfo")
    succeed
  }

}
