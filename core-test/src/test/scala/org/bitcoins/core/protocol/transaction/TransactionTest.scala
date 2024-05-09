package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  WitnessTxSigComponentP2SH,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCase
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.crypto.{CryptoUtil, ECPublicKey}
import org.bitcoins.testkitcore.gen.TransactionGenerators
import org.bitcoins.testkitcore.util.{
  BitcoinSUnitTest,
  TestUtil,
  TransactionTestUtil
}
import scodec.bits._

class TransactionTest extends BitcoinSUnitTest {
  behavior of "Transaction"

  it must "have serialization symmetry" in {
    forAll(TransactionGenerators.transaction) { tx =>
      val result = Transaction(tx.hex) == tx
      assert(result)
    }
  }

  it must "always have TXID of a base transaction be SHA256(SHA256(hex))" in {
    forAll(TransactionGenerators.baseTransaction) { (btx: BaseTransaction) =>
      assert(btx.txId == CryptoUtil.doubleSHA256(btx.bytes))
    }
  }

  it must
    "wtxid must be the same as the SHA256(SHA256(hex)) of a wtx and " +
    "wtxid and txid are not the same for witness transactions" in {
      forAll(TransactionGenerators.witnessTransaction) {
        (wtx: WitnessTransaction) =>
          assert(wtx.wTxId == CryptoUtil.doubleSHA256(wtx.bytes))
          assert(wtx.wTxId != wtx.txId)
      }
    }

  it must "derive the correct txid from the transaction contents" in {

    // https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val tx = BaseTransaction(
      "01000000020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffffd11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff02500f1e00000000001976a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac204e0000000000001976a914321908115d8a138942f98b0b53f86c9a1848501a88ac00000000"
    )

    tx.txId.flip.bytes must be(
      hex"cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a"
    )
  }

  it must "have an empty transaction with the correct fields" in {
    EmptyTransaction.inputs.isEmpty must be(true)
    EmptyTransaction.outputs.isEmpty must be(true)
    EmptyTransaction.lockTime must be(TransactionConstants.lockTime)
    EmptyTransaction.txId.hex must be(
      "0000000000000000000000000000000000000000000000000000000000000000"
    )
  }

  it must "calculate the size of a transaction correctly" in {
    val rawTx = TestUtil.rawTransaction
    val tx = Transaction(rawTx)
    // size is in bytes so divide by 2
    assert(tx.byteSize == tx.totalSize)
    tx.byteSize must be(rawTx.size / 2)
  }

  it must "serialize and deserialize a tx" in {
    val rawTx =
      "c252e03b00018e34c6cc18a922f4232103904c9002c72238fbd0ef2e8500d305402b8eb7dcf44e0923a3ec5a307b0ba5e0ac2f50eed4"
    val tx = Transaction(rawTx)
    tx.hex must be(rawTx)
  }

  it must "deserialize and serialize a base transaction with no inputs" in {
    // this should be considered a base transaction since there is no script witnesses
    val hex = "02000000" + // version
      "0001" + // 0 inputs, 1 output
      "00e1f50500000000" + // 1BTC
      "17a9148fe46e05e329badba1c390a5ea2c0ad7de2059cd87" + // spk
      "00000000" // locktime
    val btx = Transaction.fromHex(hex)
    btx.isInstanceOf[BaseTransaction] must be(true)
    btx.hex must be(hex)
  }

  it must "serialize and deserialize a large tx" in {
    val rawTx =
      "0e2fddd0071fc32e0849ef3d3f6024aa6d73fa1eb91e3daad5a5dcfde8d45a376bc2f274b207d7017e8346304402203f7973c50fa84ab8960d5895bfcc73365101cc3967fa39528a94ab5e94de218d02207d8fc26f806d26407dd13f52be1d40c90b403690cce3933f71de57607a78848a2103bf87039d25c947357b31d005575d75071ddd6e97017e9efa57559b6a5daa03af1976a9143b75df7c44a47fed51374aef67bb7e7ae071b0a788acd3f37759c999d6f325fa7fb6445a5c6989d2fcee2b60c83cc1dd167d189488f74b09eade054ef5304847304502210087ebadf23475d287824ab171addc39907f5c93d14a5b463cbcc37805cd37cdaa02206cbca1f7768e99b65bc7a03d64056f1ce8c86ab78cbc7c19d49dab2c7084f5a830204fe815b8c4937864464ade73d3869e8888569e976742b666c742e8c60a46e5e14756035ccc736946304402204c544118e309de16cde7bc69e8018688aceb0a329e935bcf5d5f6aa8904937ad022043bee0a74e0cd9a6317d2abdab6cff004a6ebff2dc3f59b75a4e90aecf83c7b32103336d83e7f45e66b6655b25a4b3ee5679785378a89a9db407360dce45b24d67e0775bfdc70000000000000000000000000000000000000000000000000000000000000000fffffffffdb20100483046022100e52e3d78998643d2987a6210972984caaf33b53e2493daaf7dda6a00c7aade80022100d92576b1a7219c49d4ae85a7e450ee039d170a7aeed5fd7b34825faaab885055473045022100977591b85fd01d0969f39bd66f84b01adf8da5879ca6fc80c474a27502d9345f02201c415141bee3504b6eb6ff08c60e1e55b519ae80191bfa2aa9f1082581d88b5a473045022100999259a53b3b692519b95f058a6a70734f77ca4752103cb777f83ff17acff4d30220383b7debacc2dec8553d73ab1d52523a56921ba3904153fb43a4509d5dff0a2b4730450220609a352f8afd4aa49fef4bd81c13df2dbae87070217958099968b652d44d40d1022100d6f9902a240a4c516479b95a4a75ee42ea31441a7163fbcfeb1879de9907d0554630440220272828412f2c0833d9328209ec476240ee8ba51453773df4639001c84657a7ed02200b75a0bc08c002be39868a596d703ce3641b243286bede8264af0df7818843cb4830460221008ead4ad119e7350ea23de18c845a2018babea3ffd135575ece8f33ebf7f6bfe5022100e5e4067b788887edec933e7776799fe2d92915cb19e21cf7145fe4577b4eb469ffffffff0000000000000000000000000000000000000000000000000000000000000000ffffffff6946304402207b1000f0aea3a8f9f3952d13a7fd5eceb3740e6a4f2543987329f1a99a0bec6c0220457fcff820bac0c0f5c65c4228b89eadd380a98a99dfa2ed7c135617323b7ff021020610f065313942890798946ed97ca12d2852f66765bfd6785f90db650bb7d62affffffff42c761d25e0f10cfc2c82280f741e63e0aa184dd28627c487d6add381949be757d48ab4e85483046022100bceb7174237d148d3be472ad2fa9b19d2db5e9256943f32ed4abc6ae603602d8022100832b11ae88981e588daa6e65fcdd732ee186c21287589f7cbf0678109c483add210325259fd900969d3c16e870eeb9ecfd2bff8519c2aa81958a8bc19be6594a2d6e1976a9140bc44e1f010f5dfa4a102319e3a2445c164ce5d088ac5b0ddd9c652982074e88b4724293e21f651804b327586396ecf411784e80f6478fb23f606d569c746a47304502200ec20fae608b985e8b65f29a9e69ec671926eb837797763bf0178a515407b28a022100a131250b8be7fb2a533d44a1acf620155cf352ba8387f4e419b5b06c2b50901d2103d46e46269fc7ed661e4c34f714cfe6cd36231a77d79b7fd17edcb1741fdab9b781b905f5003c4edbe4"
    val tx = Transaction(rawTx)
    tx.hex must be(rawTx)
    Transaction(tx.hex) must be(tx)
  }

  it must "calculate the correct txid and wtxid for a witness transaction" in {
    // from http://tapi.qbit.ninja/tx/d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c
    val bytes =
      hex"0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f8560100000000ffffffff0100b4f505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100df7b7e5cda14ddf91290e02ea10786e03eb11ee36ec02dd862fe9a326bbcb7fd02203f5b4496b667e6e281cc654a2da9e4f08660c620a1051337fa8965f727eb19190121038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990ac00000000"
    val wtx = WitnessTransaction.fromBytes(bytes)
    val expected =
      "d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c"
    val wTxExpected = CryptoUtil.doubleSHA256(bytes)
    wtx.txId.flip.hex must be(expected)
    wtx.txIdBE.hex must be(expected)
    wtx.wTxId must be(wTxExpected)
  }

  it must "parse a witness coinbase tx correctly" in {
    // txid is b3974ba615f60f48b4c558a4080810fa5064cfcb88e59b843e7fd552b3f4b3d1 on testnet
    val hex =
      "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2003e02e10130e6d696e65642062792062636f696e585bd0620000000006d50200ffffffff03cddf17000000000017a9146859969825bb2787f803a3d0eeb632998ce4f50187848c3b090000000017a9146859969825bb2787f803a3d0eeb632998ce4f501870000000000000000356a24aa21a9ed309cfb38d1015c266667d5b7888c83def872a531b8ac277fe8df623c32b562b50e6d696e65642062792062636f696e0120000000000000000000000000000000000000000000000000000000000000000000000000"
    val tx = Transaction(hex)
    val wtx = tx.asInstanceOf[WitnessTransaction]
    wtx.inputs.size must be(1)
    wtx.outputs.size must be(3)
    val witCommitment = wtx.outputs.last
    witCommitment.scriptPubKey.isInstanceOf[WitnessCommitment] must be(true)
    wtx.hex must be(hex)
  }

  it must "calculate weight and vsize correctly" in {
    // non segwit 27fd54c26042e72210eb519d631cec0fa93e676d8cf996ec5423bc7e2d14359d
    val hex =
      "0200000008c590221c1971757a12de2c65cd49f38c6cac48e59a60bb7062cb0b26fb142c7a000000006a47304402200bf1b5d42ae0b860aaf77e91bf9e0f5d95662080b462ffaeaef3fdd4528cd42e02201a400b2adc6672c77068306e063f0421731d3f4aa4b9160837a30446d4c8c12f01210321da332a698189f31e188d7b735a8871ad816ea81b375336c273e7fec5dc9c2e00000000060e343f7cba7c38b1aea0a5eb38095c6c1eb21aaa1d3de7019abea433a62f76010000006a47304402207b2ed3ac323a63cba132556f2120d0669207f661a71212cbbf2b5f89a5f2d0360220651fbf59b5b5da0452146d95fbeedaf0c91101af221b818361421aa75b562616012102fa61b8d3acbd3b1364cd48fb70b5a84bd16e8087c421169bdc458f950f95c4ae00000000ea3c63b3e9e8f8cce011d607ffcd14b83ccdf7c1fd5df02354701ecd8223e600000000006a473044022034fea16d8db904437aab1c2bc4e3874f43bbc21eea9819a805f44a38eb51401a022050f0b321e05e204c22035772f161c6a1782628d14f7ac7ce464064645e027bb2012103294607de1909df8c7c70405b661b15db23373bb01cb17cc591cf19e3e184bc70000000001559693b1207392115bcafa76dc244038b4202bdd84a25312e3a10e4f3a66cb6010000006a473044022049df75532fa7db44f166f3b3c1dca3fce882444246e4a2451a1df56805b06ebc022079f8ffc02e16cda2f5340f3ed04a7d9e53366de5ee5739989dddda155202137901210356fc739e39c3554265af065963e9d1ba4983569b0235e03734f063bb6dd7283f00000000e47ef81325dc635f3d327027842a3fd6ef8cccf8eb12fadc31f2e4ea53300d08000000006a47304402206383a91058d6ac67fb1c0f2feca6e694d32bf6ea4b1320118a3d222fe77aa89102201cc9386c5ad21a7c3a2c504b4bebe0757c4c327d554caccda51afb2c15b8ff9c012103a1f9c896e58fe1130fd99562e76eb34767c1d58e5b0535c3ac1464964cf1c74700000000414e174eed5931d7627d76a18213d234866652d0c791c41013b5439e407b8933010000006a47304402203b4db0a1888752b5fdc37ad24ad864282b13a6a898237341fc4ecbfbb88db1f202203d1e6e6ce9692a1a8a5aa72f2a11bb0f16bf149a303e0a8d4f4e042124507bb3012102f60bdfbb626916965b8c7c9b7e03300cc52afb7bec7843898b6822c4d94296c800000000a67cb436ece24d133a731ae71545bbda68518fc119a7a09f83ef65052f01df04010000006b483045022100e6dba05a228da5c44a16c7f2936e0056bd0b27106b91632d0a975ddf3d0b022a0220552605ef5936c6af9e15c6f81cda980afd088564bfb63560db572cae5baf38750121033d91b05ab07af5e58179cd26b806efe3671ab03bfde80a75f56ce4bcc5458d9800000000f424d7c0714fa8fd66049eaaec80868084343bc0e9889afc72b6debdf33498a7000000006a47304402206b48ffbca3abdcb95067949b6db6ea89f6b17deb2adea930ad512db5b1fa5f9c02204025fc165bc4c95400689b03037324358e55d69410b5b94277f004035caa58c50121036ba893997c092e90dba979b224b878e8c3c21b5fd5b737ba55d5f6a5196a4c210000000001c0860e00000000001976a914a8fc6e80e43c99e0918f8e043f0e3a48e842095088ac00000000"
    val tx = Transaction(hex)
    tx.weight must be(4884)
    tx.vsize must be(1221)

    // segwit c586389e5e4b3acb9d6c8be1c19ae8ab2795397633176f5a6442a261bbdefc3a (sipa 3rd segwit tx)
    val hex2 =
      "0200000000010140d43a99926d43eb0e619bf0b3d83b4a31f60c176beecfb9d35bf45e54d0f7420100000017160014a4b4ca48de0b3fffc15404a1acdc8dbaae226955ffffffff0100e1f5050000000017a9144a1154d50b03292b3024370901711946cb7cccc387024830450221008604ef8f6d8afa892dee0f31259b6ce02dd70c545cfcfed8148179971876c54a022076d771d6e91bed212783c9b06e0de600fab2d518fad6f15a2b191d7fbd262a3e0121039d25ab79f41f75ceaf882411fd41fa670a4c672c23ffaf0e361a969cde0692e800000000"
    val tx2 = WitnessTransaction(hex2)
    tx2.hex must be(hex2)
    tx2.byteSize must be(216)
    tx2.weight must be(534)
    tx2.vsize must be(134)
    tx2.baseSize must be(106)
  }

  it must "parse a transaction with an OP_PUSHDATA4 op code but not enough data to push" in {
    val hex =
      "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff2a03f35c0507062f503253482ffe4ecb3b55fefbde06000963676d696e6572343208040000000000000000ffffffff0100f90295000000001976a91496621bc1c9d1e5a1293e401519365de820792bbc88ac00000000"
    val btx = BaseTransaction.fromHex(hex)
    btx.hex must be(hex)
  }
  it must "read all of the tx_valid.json's contents and return ScriptOk" in {
    val lines = JsonTestVectors.valid
    val testCasesOpt =
      upickle.default.read[Seq[Option[CoreTransactionTestCase]]](lines)
    val testCases: Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (outPoint, scriptPubKey, amountOpt) <- testCase.creditingTxsInfo
      tx = testCase.spendingTx
      (input, inputIndex) =
        findInput(tx, outPoint).getOrElse((EmptyTransactionInput, 0))
    } yield {
      assert(
        outPoint.txId == input.previousOutput.txId,
        s"""
           |OutPoint txId not the same as input prevout txid
           |outPoint.txId: ${outPoint.txId}
           |input prevout txid: ${input.previousOutput.txId}
           |""".stripMargin
      )
      val txSigComponent = amountOpt match {
        case Some(amount) =>
          scriptPubKey match {
            case p2sh: P2SHScriptPubKey =>
              tx match {
                case btx: NonWitnessTransaction =>
                  BaseTxSigComponent(
                    transaction = btx,
                    inputIndex = UInt32(inputIndex),
                    output = TransactionOutput(amount, p2sh),
                    flags = testCase.flags
                  )
                case wtx: WitnessTransaction =>
                  WitnessTxSigComponentP2SH(
                    transaction = wtx,
                    inputIndex = UInt32(inputIndex),
                    output = TransactionOutput(amount, p2sh),
                    flags = testCase.flags
                  )
              }
            case wit: WitnessScriptPubKey =>
              tx match {
                case btx: NonWitnessTransaction =>
                  BaseTxSigComponent(
                    transaction = btx,
                    inputIndex = UInt32(inputIndex),
                    output = TransactionOutput(amount, wit),
                    flags = testCase.flags
                  )
                case wtx: WitnessTransaction =>
                  WitnessTxSigComponentRaw(
                    transaction = wtx,
                    inputIndex = UInt32(inputIndex),
                    output = TransactionOutput(amount, wit),
                    flags = testCase.flags
                  )
              }
            case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey | _: CLTVScriptPubKey |
                _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey) =>
              val output = TransactionOutput(amount, x)

              BaseTxSigComponent(tx, UInt32(inputIndex), output, testCase.flags)
          }
        case None =>
          BaseTxSigComponent(
            transaction = tx,
            inputIndex = UInt32(inputIndex),
            output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
            flags = testCase.flags
          )
      }
      val program = PreExecutionScriptProgram(txSigComponent)
      withClue(s"${testCase.raw} input index: $inputIndex") {
        ScriptInterpreter.run(program) must equal(ScriptOk)
      }
    }
  }

  it must "read all of the tx_invalid.json's contents and return a ScriptError" in {
    val lines = JsonTestVectors.invalid
    val testCasesOpt =
      upickle.default.read[Seq[Option[CoreTransactionTestCase]]](lines)
    val testCases: Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
    } yield {
      val txInputValidity: Seq[Boolean] = for {
        (outPoint, scriptPubKey, amountOpt) <- testCase.creditingTxsInfo
        tx = testCase.spendingTx
        (input, inputIndex) =
          findInput(tx, outPoint).getOrElse((EmptyTransactionInput, 0))
      } yield {
        val isValidTx = ScriptInterpreter.checkTransaction(tx)
        if (isValidTx) {
          val txSigComponent = amountOpt match {
            case Some(amount) =>
              scriptPubKey match {
                case _: P2SHScriptPubKey =>
                  tx match {
                    case btx: NonWitnessTransaction =>
                      BaseTxSigComponent(
                        transaction = btx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, scriptPubKey),
                        flags = testCase.flags
                      )
                    case wtx: WitnessTransaction =>
                      WitnessTxSigComponentP2SH(
                        transaction = wtx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, scriptPubKey),
                        flags = testCase.flags
                      )
                  }
                case wit: WitnessScriptPubKey =>
                  tx match {
                    case btx: NonWitnessTransaction =>
                      BaseTxSigComponent(
                        transaction = btx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, wit),
                        flags = testCase.flags
                      )
                    case wtx: WitnessTransaction =>
                      WitnessTxSigComponentRaw(
                        transaction = wtx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, wit),
                        flags = testCase.flags
                      )
                  }
                case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                    _: P2PKWithTimeoutScriptPubKey |
                    _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey |
                    _: CSVScriptPubKey | _: CLTVScriptPubKey |
                    _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
                    _: WitnessCommitment | EmptyScriptPubKey) =>
                  BaseTxSigComponent(
                    transaction = tx,
                    inputIndex = UInt32(inputIndex),
                    output = TransactionOutput(amount, x),
                    flags = testCase.flags
                  )
              }
            case None =>
              BaseTxSigComponent(
                transaction = tx,
                inputIndex = UInt32(inputIndex),
                output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                flags = testCase.flags
              )
          }
          val program = PreExecutionScriptProgram(txSigComponent)
          ScriptInterpreter.run(program) == ScriptOk
        } else {
          isValidTx
        }
      }
      withClue(testCase.raw) {
        // only one input is required to be false to make the transaction invalid
        txInputValidity.contains(false) must be(true)
      }
    }
  }

  it must "check transaction with two out point referencing the same tx with different indexes" in {
    val hex =
      "0200000002924942b0b7c12ece0dc8100d74a1cd29acd6cfc60698bfc3f07d83890eec20b6000000006a47304402202831d3708867f9bfb4268690cbcf97a686ccec1f5a4334cf0256fd442a88d0b802206fa8fa5550df8bfcc9c31cc8b6aad035be68b767b67e2b823f844680a79349650121038991058ce7ef4b00194e8426e3630dffd32822f819c150938f26113ba751c9a100000000924942b0b7c12ece0dc8100d74a1cd29acd6cfc60698bfc3f07d83890eec20b6010000006a47304402202e4cf01174afb9f97b0ab8f24c64c796ae4b3bb91d1838099bf262e8842e6480022006399d769d6d4ba099c2d3188f62caa5b51f572e7c2775a9bc23495c020dd1090121038991058ce7ef4b00194e8426e3630dffd32822f819c150938f26113ba751c9a1000000000288130000000000001976a914cc1fe3e943bd91e0e263f08a93f0d2a5299a7e5e88ac32600000000000001976a914af84620f44464d1a404386485885d1cd9e5d396d88ac00000000"
    val btx = BaseTransaction.fromHex(hex)
    ScriptInterpreter.checkTransaction(btx) must be(true)
  }

  it must "construct a base transaction that pays to a taproot address" in {
    val output = TransactionOutput(
      Bitcoins.one,
      TransactionTestUtil.bech32mAddr.scriptPubKey
    )
    val btx = BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector(
        TransactionInput(
          EmptyTransactionOutPoint,
          ScriptSignature.empty,
          UInt32.max
        )
      ),
      Vector(output),
      TransactionConstants.lockTime
    )
    assert(BaseTransaction.fromHex(btx.hex) == btx)
  }

  it must "construct a witness transaction that pays to a taproot address" in {
    val output = TransactionOutput(
      Bitcoins.one,
      TransactionTestUtil.bech32mAddr.scriptPubKey
    )
    val inputs: Vector[TransactionInput] = Vector(
      TransactionInput(
        EmptyTransactionOutPoint,
        ScriptSignature.empty,
        UInt32.max
      )
    )

    val pubKeyWit = ECPublicKey.freshPublicKey
    val p2WPKHWitnessV0 = P2WPKHWitnessV0(pubKeyWit)
    val witness: TransactionWitness =
      TransactionWitness(Vector(p2WPKHWitnessV0))
    val wtx = WitnessTransaction(
      TransactionConstants.validLockVersion,
      inputs,
      Vector(output),
      TransactionConstants.lockTime,
      witness
    )
    assert(WitnessTransaction.fromHex(wtx.hex) == wtx)
  }

  it must "parse 66cc4de192001d970244ffe32896282a1994fef80f01d35b216033aeacac1651" in {
    val hex =
      "01000000000101d498f5f6e3a4f04e88170c4dfaa8acef0cf9b949086c15e23d75d98a5d516ec50000000000ffffffff02b6ca050000000000225120af68875a973914dd815fff548a9104eeb9e44bee55b2b1a7428bab71ef33bd362b0103000000000017a914f92c8425eb54c3ef38e507a786297d2648441c0e87" +
        "014104720fcb29324a375b02b26c30cd45526dc7bdf668a59a4340f8bcf89275fafac91d0f763945373d17210650d717f5a1c465046c44c0ba2da74d6a76f61d1c370100000000"
    val tx = Transaction.fromHex(hex)
    assert(
      tx.txIdBE.hex == "66cc4de192001d970244ffe32896282a1994fef80f01d35b216033aeacac1651"
    )
    assert(tx.hex == hex)
  }

  it must "parse e597ad88c1366e53743ac201d0156abda38cde9720b80baf7328ce9fa677772d" in {
    val hex =
      "02000000000102d003fe1e06554e70a8e6a7026407c54f9ad1c3ca29a5486fd9825c74534cf338070000006a47304402200fa8b5e8d4b32aaf060ceb1de381d2cb9481b95591ac086f548869dabe2b625402207e347389e0a92f48c3f7fd3f4ae0e05878c7fa55584941dec3f347fa9c87ea8c0121039766e910a95b9be9b6504bfc1585c1c739a295f29099c9ee20d8fd91f58191bfffffffff22c93527a49ee479e05fb00023430f519402ee70c1d1871c5df0f896be70bb1f0000000000ffffffff02fd423800000000001976a91425ff4d7161289d9950d332a93b226abf429ff97188ac62060000000000001600144b109941eba2e10e5feb034342e9124c9edf9d310002473044022034b207e3008680ca7e3785d1a4fcbbd4f167ee8dad86a506ef886928f5072ce3022055647f8a2352e1c09fd9127fffc91c7918f90c5a8870e07760a744a4412df701012102dfac2fa93040d21e83c98f3b2d6b0a1dc95921c301118a6597e373d5ba6bc08300000000"
    val tx = Transaction.fromHex(hex)
    assert(
      tx.txIdBE.hex == "e597ad88c1366e53743ac201d0156abda38cde9720b80baf7328ce9fa677772d"
    )
    assert(tx.hex == hex)
  }

  it must "parse 37d5ec4bca7bd077992a6dd8679ab676a22986e63ebaf2c6ea1aebe5e5f5e817" in {
    // has an invalid x coordinate in the wit v1 spk
    // https://blockstream.info/tx/37d5ec4bca7bd077992a6dd8679ab676a22986e63ebaf2c6ea1aebe5e5f5e817

    val hex =
      "0100000001a76e1bb0e40d989cf3613c5b73bff552adaa09bdf79264cc0c44b968502db3be01000000fdfd000047304402200b0e9309480146ed5923a14cb3cd607facf9cf7b4f51ac9d448e0c8c3030aa5602201850641a5dd1c002e6ed9b723404a069a002608f45f77af1e3eb96e911d17afd01483045022100c7063f26164395e0ac8127b827cf6ad6a472fa8516197ef9ff565d7465b08dff022073329ded25b9559ce0dfef313ed084f9e294da47ddc814d2eab8839a8a4a9f16014c69522102b09c72ee2fa77abc24613b227d1cb6944c70af7f96711d5f1c4675daaa7554d221029787faf77569cdc59db5a34e0e9f552b9950b61d593e797ae12ac1d69eb5f98821022e9df5da07c71f7e8f5f639a28900d967714f2ae72b096c69c93fd76837b01a753aeffffffff02204e000000000000225120658204033e46a1fa8cceb84013cfe2d376ca72d5f595319497b95b08aa64a97014be00000000000017a914d06f57927ef98f7dfd25e9abdf5de65e1e7215698700000000"
    val btx = BaseTransaction.fromHex(hex)
    val spk = btx.outputs(0).scriptPubKey
    assert(spk.isInstanceOf[UnassignedWitnessScriptPubKey])
  }

  it must "parse a05213473724e2e7f489ae0cbe1d17eef4f6c1b9806e33e5115d85e71d81e994" in {
    // cause of https://github.com/bitcoin-s/bitcoin-s/issues/4455
    val hex =
      "02000000000101df1d56ff246619d49d9f416ae742c4fc0fc247729748818cae2cd869394ff04500000000003b0000000163120000000000002200200dfe4629adca8af581bf9d8fc8384b9c2ee898a39707930017b419cf31b965d7014050795800afc8005c6d57ddb994ffa8d0e343549b4abd2ce38671cf14c92769091deee4eaa7704e84536f8e0de52789c8cc8e679d21c4ec060f5d92d51ed9562e00000000"
    val tx = Transaction.fromHex(hex)
    assert(
      tx.txIdBE.hex == "a05213473724e2e7f489ae0cbe1d17eef4f6c1b9806e33e5115d85e71d81e994"
    )
  }

  it must "parse f6991ce32ff870ae692571764e628dec9c633bf63a7190d52fc5a0ced6c5203b" in {
    // cause of https://github.com/bitcoin-s/bitcoin-s/issues/4489
    val hex =
      "02000000000101eb7a5073cc587ccab25b97fee2ff9c08f9461da9240ce69fcf133bacf6afdcd40000000000ffffffff0100000000000000001e6a1c53656520616e6e657821204c6f6f6b206d61206d6f72652064617461054184c66fe6dac66f09290320e31f2851b9dbe3849041f0eca2549faf92f4ae142ae7f9134b7cd4b9be97c1b26bfa6ec61c2e32b340c65c6aee8c3d1a5398f4a523014059155471452f00c6ccc869a4db103985036fe98b5d9e11b95d3fd01396e231dc791a56eee707149629df569f113263a92b9daf4336f3acd6c6f34208895ec5974420febe583fa77e49089f89b78fa8c116710715d6e40cc5f5a075ef1681550dd3c4ad20d0fa46cb883e940ac3dc5421f05b03859972639f51ed2eccbf3dc5a62e2e1b15ac41c02e44c9e47eaeb4bb313adecd11012dfad435cd72ce71f525329f24d75c5b9432774e148e9209baf3f1656a46986d5f38ddf4e20912c6ac28f48d6bf747469fb1075073656372657400000000"
    val tx = Transaction.fromHex(hex)
    assert(
      tx.txIdBE.hex == "f6991ce32ff870ae692571764e628dec9c633bf63a7190d52fc5a0ced6c5203b"
    )
  }

  it must "parse 4b662a144400fa2d47d94f7380b50f299de8de541713a3efd8f79e6450640968" in {
    val hex =
      "0100000001bcdff6bc6a8d2876448ad6d59a8c8ea049466299a7936d378996f8c59e794049000000008b4830450220588a4a73324eca771665d3e9af0f5a9c55af4d4f2e9843b25e5738ea8f6d19e1022100ae10a39ce1eb15a67904f6e0183e94eb0435980c401853640bbdef806f587a800141040fba36883926dc3e156df0df4a7873ebadd39fcbe995742b59214b456a08770c836a5120badbf5eadb7c449ae0039f53ad82b8a7286dbeafa719e110052152aeffffffff0280698bf3000000001976a914ee69a497c634509a2aa72efae9e56d7b33b19e6c88ac80969800000000001976a91413fc5a59dd90fd25eaee0775f8aa00c95a379a0b88ac00000000"
    val tx = Transaction.fromHex(hex)

    assert(tx.hex == hex)
  }

  it must "parse 1c1f50eb03c28c56ea0f2abdff483e836a8110f365cd678af1ae892a550f71eb" in {
    val hex =
      "0100000001c5b9f89aeebb94f6838d9e8b0c0876dd7b62249a14f7c3acd466a4a6682c2530010000006b483045022068d7ba61ae4670fe857ca4617413e2a8f666c7a30dbee9ad52a77406f62fd4bb0221009ed0034c8005b17b8e85e5b5d9cd35a79914665863c1ea85201ff0b42ec1f48b01210291b3da73ea3ce05d942315135d10532b58175568092116da909da0cb42006a54ffffffff0248cf9700000000001976a91452b5f62ff6a34dc0937baa262314649b22caebec88ace8030000000000001714e41346a0f116a7d04984c2780a396b18b0e47ea7b17500000000"
    val tx = Transaction.fromHex(hex)
    assert(tx.hex == hex)
  }

  it must "parse decb09c41bc76bad8e006d92cf9f8c7ad4114e441a9cc6daf149e1495593a82f" in {
    val hex =
      "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff5303de1304040007762f44124d696e656420627920425443204775696c642cfabe6d6d4da12b2799cc8b6bba921e104cc9054fd1da40ed6c1b6287efd8460475f2a0440100000000000000080247961b56ae0000ffffffff018759b396000000001976a91427a1f12771de5cc3b73941664b2537c15316be4388ac00000000"
    val tx = Transaction.fromHex(hex)
    assert(tx.hex == hex)
  }

  it must "parse 109c3834d74fc4788b42ca884f89c3fe3f95c9bba6d1b5b10ec4d3f426a7d12a" in {
    val hex =
      "0100000001214a8d0ae8eecd5a42dccfe5d9e9ab4738f28237651ed697139836007419e56b000000006b483045022100f501ce60b5b26d6b24f987378b32d64ce47124004f4e63be06a7748fcc078c160220444619b548700b6105bc896b36017d0f01999f5020ca1eeca3c313e9c2e3252c012103bc161c3a4663978ebfb7561588c85414fbcce0222ed19cdd525267e4ba1bbd37ffffffff0280841e0000000000035152ae50604302000000001976a914c2e88b055bdaae6c7996db9784433e9e7cbc96e088ac00000000"
    val tx = Transaction.fromHex(hex)
    assert(tx.hex == hex)
  }

  it must "parse 3a8dd04bc1f8179d0b85c8e1a1e89d058833ae64a9a8c3681da3ca329297beb1" in {
    // from testnet
    val hex =
      "0100000001e2643a9a0c05870ef7acdd91dce45ab3622d7a37302b9bbe87841c1d732cc3c4000000006a473044022043b5e41451ef06608dff4293249132a4f79184729505fbbdc45a5058442c418702207fbc1302fd3aa6c5e45787e8acdc9b79e0b54c60e10f8c5ecf9f192c24ef1a570121028c15a48d980aa15a48ca50636c88cbb2b4d38c549945842fb3fde2a5725d5ec3ffffffff033069c901000000001976a914b594f541ea352015c97f26413d46a2c31c28551c88ac0c030000000000002551211c434e54525052545900000000000000000000000100000000000927c00000000052ae0c030000000000001976a914f1f57da6302ad32eecada4b144d532122dea59dd88ac00000000"
    val tx = Transaction.fromHex(hex)
    assert(tx.hex == hex)
  }

  private def findInput(
      tx: Transaction,
      outPoint: TransactionOutPoint
  ): Option[(TransactionInput, Int)] = {
    tx.inputs.zipWithIndex.find { case (input, _) =>
      input.previousOutput == outPoint
    }
  }
}

object JsonTestVectors {

  val valid: String = {
    val str1 =
      "[\n[\"The following are deserialized transactions which are valid.\"],\n[\"They are in the form\"],\n[\"[[[prevout hash, prevout index, prevout scriptPubKey, amount?], [input 2], ...],\"],\n[\"serializedTransaction, verifyFlags]\"],\n[\"Objects that are only a single string (like this one) are ignored\"],\n\n[\"The following is 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63\"],\n[\"It is of particular interest because it contains an invalidly-encoded signature which OpenSSL accepts\"],\n[\"See http://r6.ca/blog/20111119T211504Z.html\"],\n[\"It is also the first OP_CHECKMULTISIG transaction in standard form\"],\n[[[\"60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1\", 0, \"1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG\"]],\n\"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000490047304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000\", \"P2SH\"],\n\n[\"The following is a tweaked form of 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63\"],\n[\"It is an OP_CHECKMULTISIG with an arbitrary extra byte stuffed into the signature at pos length - 2\"],\n[\"The dummy byte is fine however, so the NULLDUMMY flag should be happy\"],\n[[[\"60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1\", 0, \"1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG\"]],\n\"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004a0048304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2bab01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000\", \"P2SH,NULLDUMMY\"],\n\n[\"The following is a tweaked form of 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63\"],\n[\"It is an OP_CHECKMULTISIG with the dummy value set to something other than an empty string\"],\n[[[\"60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1\", 0, \"1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG\"]],\n\"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004a01ff47304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000\", \"P2SH\"],\n\n[\"As above, but using a OP_1\"],\n[[[\"60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1\", 0, \"1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG\"]],\n\"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000495147304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000\", \"P2SH\"],\n\n[\"As above, but using a OP_1NEGATE\"],\n[[[\"60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1\", 0, \"1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG\"]],\n\"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000494f47304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000\", \"P2SH\"],\n\n[\"The following is c99c49da4c38af669dea436d3e73780dfdb6c1ecf9958baa52960e8baee30e73\"],\n[\"It is of interest because it contains a 0-sequence as well as a signature of SIGHASH type 0 (which is not a real type)\"],\n[[[\"406b2b06bcd34d3c8733e6b79f7a394c8a431fbf4ff5ac705c93f4076bb77602\", 0, \"DUP HASH160 0x14 0xdc44b1164188067c3a32d4780f5996fa14a4f2d9 EQUALVERIFY CHECKSIG\"]],\n\"01000000010276b76b07f4935c70acf54fbf1f438a4c397a9fb7e633873c4dd3bc062b6b40000000008c493046022100d23459d03ed7e9511a47d13292d3430a04627de6235b6e51a40f9cd386f2abe3022100e7d25b080f0bb8d8d5f878bba7d54ad2fda650ea8d158a33ee3cbd11768191fd004104b0e2c879e4daf7b9ab68350228c159766676a14f5815084ba166432aab46198d4cca98fa3e9981d0a90b2effc514b76279476550ba3663fdcaff94c38420e9d5000000000100093d00000000001976a9149a7b0f3b80c6baaeedce0a0842553800f832ba1f88ac00000000\", \"P2SH\"],\n\n[\"A nearly-standard transaction with CHECKSIGVERIFY 1 instead of CHECKSIG\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"DUP HASH160 0x14 0x5b6462475454710f3c22f5fdf0b40704c92f25c3 EQUALVERIFY CHECKSIGVERIFY 1\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000006a473044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a012103ba8c8b86dea131c22ab967e6dd99bdae8eff7a1f75a2c35f1f944109e3fe5e22ffffffff010000000000000000015100000000\", \"P2SH\"],\n\n[\"Same as above, but with the signature duplicated in the scriptPubKey with the proper pushdata prefix\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"DUP HASH160 0x14 0x5b6462475454710f3c22f5fdf0b40704c92f25c3 EQUALVERIFY CHECKSIGVERIFY 1 0x47 0x3044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a01\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000006a473044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a012103ba8c8b86dea131c22ab967e6dd99bdae8eff7a1f75a2c35f1f944109e3fe5e22ffffffff010000000000000000015100000000\", \"P2SH\"],\n\n[\"The following is f7fdd091fa6d8f5e7a8c2458f5c38faffff2d3f1406b6e4fe2c99dcc0d2d1cbb\"],\n[\"It caught a bug in the workaround for 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63 in an overly simple implementation. In a signature, it contains an ASN1 integer which isn't strict-DER conformant due to being negative, which doesn't make sense in a signature. Before BIP66 activated, it was a valid signature. After it activated, it's not valid any more.\"],\n[[[\"b464e85df2a238416f8bdae11d120add610380ea07f4ef19c5f9dfd472f96c3d\", 0, \"DUP HASH160 0x14 0xbef80ecf3a44500fda1bc92176e442891662aed2 EQUALVERIFY CHECKSIG\"],\n[\"b7978cc96e59a8b13e0865d3f95657561a7f725be952438637475920bac9eb21\", 1, \"DUP HASH160 0x14 0xbef80ecf3a44500fda1bc92176e442891662aed2 EQUALVERIFY CHECKSIG\"]],\n\"01000000023d6cf972d4dff9c519eff407ea800361dd0a121de1da8b6f4138a2f25de864b4000000008a4730440220ffda47bfc776bcd269da4832626ac332adfca6dd835e8ecd83cd1ebe7d709b0e022049cffa1cdc102a0b56e0e04913606c70af702a1149dc3b305ab9439288fee090014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff21ebc9ba20594737864352e95b727f1a565756f9d365083eb1a8596ec98c97b7010000008a4730440220503ff10e9f1e0de731407a4a245531c9ff17676eda461f8ceeb8c06049fa2c810220c008ac34694510298fa60b3f000df01caa244f165b727d4896eb84f81e46bcc4014104266abb36d66eb4218a6dd31f09bb92cf3cfa803c7ea72c1fc80a50f919273e613f895b855fb7465ccbc8919ad1bd4a306c783f22cd3227327694c4fa4c1c439affffffff01f0da5200000000001976a914857ccd42dded6df32949d4646dfa10a92458cfaa88ac00000000\", \"P2SH\"],\n\n[\"The following tests for the presence of a bug in the handling of SIGHASH_SINGLE\"],\n[\"It results in signing the constant 1, instead of something generated based on the transaction,\"],\n[\"when the input doing the signing has an index greater than the maximum output index\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000200\", 0, \"1\"], [\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"DUP HASH160 0x14 0xe52b482f2faa8ecbf0db344f93c84ac908557f33 EQUALVERIFY CHECKSIG\"]],\n\"01000000020002000000000000000000000000000000000000000000000000000000000000000000000151ffffffff0001000000000000000000000000000000000000000000000000000000000000000000006b483045022100c9cdd08798a28af9d1baf44a6c77bcc7e279f47dc487c8c899911bc48feaffcc0220503c5c50ae3998a733263c5c0f7061b483e2b56c4c41b456e7d2f5a78a74c077032102d5c25adb51b61339d2b05315791e21bbe80ea470a49db0135720983c905aace0ffffffff010000000000000000015100000000\", \"P2SH\"],\n\n[\"An invalid P2SH Transaction\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0x7a052c840ba73af26755de42cf01cc9e0a49fef0 EQUAL\"]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000009085768617420697320ffffffff010000000000000000015100000000\", \"NONE\"],\n\n[\"A valid P2SH Transaction using the standard transaction type put forth in BIP 16\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0x8febbed40483661de6958d957412f82deed8e2f7 EQUAL\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100c66c9cdf4c43609586d15424c54707156e316d88b0a1534c9e6b0d4f311406310221009c0fe51dbc9c4ab7cc25d3fdbeccf6679fe6827f08edf2b4a9f16ee3eb0e438a0123210338e8034509af564c62644c07691942e0c056752008a173c89f60ab2a88ac2ebfacffffffff010000000000000000015100000000\", \"P2SH\"],\n\n[\"Tests for CheckTransaction()\"],\n[\"MAX_MONEY output\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0x32afac281462b822adbec5094b8d4d337dd5bd6a EQUAL\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100e1eadba00d9296c743cb6ecc703fd9ddc9b3cd12906176a226ae4c18d6b00796022100a71aef7d2874deff681ba6080f1b278bac7bb99c61b08a85f4311970ffe7f63f012321030c0588dc44d92bdcbf8e72093466766fdc265ead8db64517b0c542275b70fffbacffffffff010040075af0750700015100000000\", \"P2SH\"],\n\n[\"MAX_MONEY output + 0 output\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0xb558cbf4930954aa6a344363a15668d7477ae716 EQUAL\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000006d483045022027deccc14aa6668e78a8c9da3484fbcd4f9dcc9bb7d1b85146314b21b9ae4d86022100d0b43dece8cfb07348de0ca8bc5b86276fa88f7f2138381128b7c36ab2e42264012321029bb13463ddd5d2cc05da6e84e37536cb9525703cfd8f43afdb414988987a92f6acffffffff020040075af075070001510000000000000000015100000000\", \"P2SH\"],\n\n[\"Coinbase of size 2\"],\n[\"Note the input is just required to make the tester happy\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000000\", -1, \"1\"]],\n\"01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff025151ffffffff010000000000000000015100000000\", \"P2SH\"],\n\n[\"Coinbase of size 100\"],\n[\"Note the input is just required to make the tester happy\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000000\", -1, \"1\"]],\n\"01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff6451515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151ffffffff010000000000000000015100000000\", \"P2SH\"],\n\n[\"Simple transaction with first input is signed with SIGHASH_ALL, second with SIGHASH_ANYONECANPAY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG\"],\n  [\"0000000000000000000000000000000000000000000000000000000000000200\", 0, \"0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG\"]],\n \"010000000200010000000000000000000000000000000000000000000000000000000000000000000049483045022100d180fd2eb9140aeb4210c9204d3f358766eb53842b2a9473db687fa24b12a3cc022079781799cd4f038b85135bbe49ec2b57f306b2bb17101b17f71f000fcab2b6fb01ffffffff0002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000\", \"P2SH\"],\n\n[\"Same as above, but we change the sequence number of the first input to check that SIGHASH_ANYONECANPAY is being followed\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG\"],\n  [\"0000000000000000000000000000000000000000000000000000000000000200\", 0, \"0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG\"]],\n \"01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000\", \"P2SH\"],\n\n[\"afd9c17f8913577ec3509520bd6e5d63e9c0fd2a5f70c787993b097ba6ca9fae which has several SIGHASH_SINGLE signatures\"],\n[[[\"63cfa5a09dc540bf63e53713b82d9ea3692ca97cd608c384f2aa88e51a0aac70\", 0, \"DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG\"],\n [\"04e8d0fcf3846c6734477b98f0f3d4badfb78f020ee097a0be5fe347645b817d\", 1, \"DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG\"],\n [\"ee1377aff5d0579909e11782e1d2f5f7b84d26537be7f5516dd4e43373091f3f\", 1, \"DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG\"]],\n \"010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000\", \"P2SH\"],\n\n [\"ddc454a1c0c35c188c98976b17670f69e586d9c0f3593ea879928332f0a069e7, which spends an input that pushes using a PUSHDATA1 that is negative when read as signed\"],\n [[[\"c5510a5dd97a25f43175af1fe649b707b1df8e1a41489bac33a23087027a2f48\", 0, \"0x4c 0xae 0x606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e207460 DROP DUP HASH160 0x14 0xbfd7436b6265aa9de506f8a994f881ff08cc2872 EQUALVERIFY CHECKSIG\"]],\n \"0100000001482f7a028730a233ac9b48411a8edfb107b749e61faf7531f4257ad95d0a51c5000000008b483045022100bf0bbae9bde51ad2b222e87fbf67530fbafc25c903519a1e5dcc52a32ff5844e022028c4d9ad49b006dd59974372a54291d5764be541574bb0c4dc208ec51f80b7190141049dd4aad62741dc27d5f267f7b70682eee22e7e9c1923b9c0957bdae0b96374569b460eb8d5b40d972e8c7c0ad441de3d94c4a29864b212d56050acb980b72b2bffffffff0180969800000000001976a914e336d0017a9d28de99d16472f6ca6d5a3a8ebc9988ac00000000\", \"P2SH\"],\n\n[\"Correct signature order\"],\n[\"Note the input is just required to make the tester happy\"],\n[[[\"b3da01dd4aae683c7aee4d5d8b52a540a508e1115f77cd7fa9a291243f501223\", 0, \"HASH160 0x14 0xb1ce99298d5f07364b57b1e5c9cc00be0b04a954 EQUAL\"]],\n\"01000000012312503f2491a2a97fcd775f11e108a540a5528b5d4dee7a3c68ae4add01dab300000000fdfe0000483045022100f6649b0eddfdfd4ad55426663385090d51ee86c3481bdc6b0c18ea6c0ece2c0b0220561c315b07cffa6f7dd9df96dbae9200c2dee09bf93cc35ca05e6cdf613340aa0148304502207aacee820e08b0b174e248abd8d7a34ed63b5da3abedb99934df9fddd65c05c4022100dfe87896ab5ee3df476c2655f9fbe5bd089dccbef3e4ea05b5d121169fe7f5f4014c695221031d11db38972b712a9fe1fc023577c7ae3ddb4a3004187d41c45121eecfdbb5b7210207ec36911b6ad2382860d32989c7b8728e9489d7bbc94a6b5509ef0029be128821024ea9fac06f666a4adc3fc1357b7bec1fd0bdece2b9d08579226a8ebde53058e453aeffffffff0180380100000000001976a914c9b99cddf847d10685a4fabaa0baf505f7c3dfab88ac00000000\", \"P2SH\"],\n\n[\"cc60b1f899ec0a69b7c3f25ddf32c4524096a9c5b01cbd84c6d0312a0c478984, which is a fairly strange transaction which relies on OP_CHECKSIG returning 0 when checking a completely invalid sig of length 0\"],\n[[[\"cbebc4da731e8995fe97f6fadcd731b36ad40e5ecb31e38e904f6e5982fa09f7\", 0, \"0x2102085c6600657566acc2d6382a47bc3f324008d2aa10940dd7705a48aa2a5a5e33ac7c2103f5d0fb955f95dd6be6115ce85661db412ec6a08abcbfce7da0ba8297c6cc0ec4ac7c5379a820d68df9e32a147cffa36193c6f7c43a1c8c69cda530e1c6db354bfabdcfefaf3c875379a820f531f3041d3136701ea09067c53e7159c8f9b2746a56c3d82966c54bbc553226879a5479827701200122a59a5379827701200122a59a6353798277537982778779679a68\"]],\n\"0100000001f709fa82596e4f908ee331cb5e0ed46ab331d7dcfaf697fe95891e73dac4ebcb000000008c20ca42095840735e89283fec298e62ac2ddea9b5f34a8cbb7097ad965b87568100201b1b01dc829177da4a14551d2fc96a9db00c6501edfa12f22cd9cefd335c227f483045022100a9df60536df5733dd0de6bc921fab0b3eee6426501b43a228afa2c90072eb5ca02201c78b74266fac7d1db5deff080d8a403743203f109fbcabf6d5a760bf87386d20100ffffffff01c075790000000000232103611f9a45c18f28f06f19076ad571c344c82ce8fcfe34464cf8085217a2d294a6ac00000000\", \"P2SH\"],\n\n[\"Empty pubkey\"],\n[[[\"229257c295e7f555421c1bfec8538dd30a4b5c37c1c8810bbe83cafa7811652c\", 0, \"0x00 CHECKSIG NOT\"]],\n\"01000000012c651178faca83be0b81c8c1375c4b0ad38d53c8fe1b1c4255f5e795c25792220000000049483045022100d6044562284ac76c985018fc4a90127847708c9edb280996c507b28babdc4b2a02203d74eca3f1a4d1eea7ff77b528fde6d5dc324ec2dbfdb964ba885f643b9704cd01ffffffff010100000000000000232102c2410f8891ae918cab4ffc4bb4a3b0881be67c7a1e7faa8b5acf9ab8932ec30cac00000000\", \"P2SH\"],\n\n[\"Empty signature\"],\n[[[\"9ca93cfd8e3806b9d9e2ba1cf64e3cc6946ee0119670b1796a09928d14ea25f7\", 0, \"0x21 0x028a1d66975dbdf97897e3a4aef450ebeb5b5293e4a0b4a6d3a2daaa0b2b110e02 CHECKSIG NOT\"]],\n\"0100000001f725ea148d92096a79b1709611e06e94c63c4ef61cbae2d9b906388efd3ca99c000000000100ffffffff0101000000000000002321028a1d66975dbdf97897e3a4aef450ebeb5b5293e4a0b4a6d3a2daaa0b2b110e02ac00000000\", \"P2SH\"],\n\n[[[\"444e00ed7840d41f20ecd9c11d3f91982326c731a02f3c05748414a4fa9e59be\", 0, \"1 0x00 0x21 0x02136b04758b0b6e363e7a6fbe83aaf527a153db2b060d36cc29f7f8309ba6e458 2 CHECKMULTISIG\"]],\n\"0100000001be599efaa4148474053c2fa031c7262398913f1dc1d9ec201fd44078ed004e44000000004900473044022022b29706cb2ed9ef0cb3c97b72677ca2dfd7b4160f7b4beb3ba806aa856c401502202d1e52582412eba2ed474f1f437a427640306fd3838725fab173ade7fe4eae4a01ffffffff010100000000000000232103ac4bba7e7ca3e873eea49e08132ad30c7f03640b6539e9b59903cf14fd016bbbac00000000\", \"P2SH\"],\n\n[[[\"e16abbe80bf30c080f63830c8dbf669deaef08957446e95940227d8c5e6db612\", 0, \"1 0x21 0x03905380c7013e36e6e19d305311c1b81fce6581f5ee1c86ef0627c68c9362fc9f 0x00 2 CHECKMULTISIG\"]],\n\"010000000112b66d5e8c7d224059e946749508efea9d66bf8d0c83630f080cf30be8bb6ae100000000490047304402206ffe3f14caf38ad5c1544428e99da76ffa5455675ec8d9780fac215ca17953520220779502985e194d84baa36b9bd40a0dbd981163fa191eb884ae83fc5bd1c86b1101ffffffff010100000000000000232103905380c7013e36e6e19d305311c1b81fce6581f5ee1c86ef0627c68c9362fc9fac00000000\", \"P2SH\"],\n\n[[[\"ebbcf4bfce13292bd791d6a65a2a858d59adbf737e387e40370d4e64cc70efb0\", 0, \"2 0x21 0x033bcaa0a602f0d44cc9d5637c6e515b0471db514c020883830b7cefd73af04194 0x21 0x03a88b326f8767f4f192ce252afe33c94d25ab1d24f27f159b3cb3aa691ffe1423 2 CHECKMULTISIG NOT\"]],\n\"0100000001b0ef70cc644e0d37407e387e73bfad598d852a5aa6d691d72b2913cebff4bceb000000004a00473044022068cd4851fc7f9a892ab910df7a24e616f293bcb5c5fbdfbc304a194b26b60fba022078e6da13d8cb881a22939b952c24f88b97afd06b4c47a47d7f804c9a352a6d6d0100ffffffff0101000000000000002321033bcaa0a602f0d44cc9d5637c6e515b0471db514c020883830b7cefd73af04194ac00000000\", \"P2SH\"],\n\n[[[\"ba4cd7ae2ad4d4d13ebfc8ab1d93a63e4a6563f25089a18bf0fc68f282aa88c1\", 0, \"2 0x21 0x037c615d761e71d38903609bf4f46847266edc2fb37532047d747ba47eaae5ffe1 0x21 0x02edc823cd634f2c4033d94f5755207cb6b60c4b1f1f056ad7471c47de5f2e4d50 2 CHECKMULTISIG NOT\"]],\n\"0100000001c188aa82f268fcf08ba18950f263654a3ea6931dabc8bf3ed1d4d42aaed74cba000000004b0000483045022100940378576e069aca261a6b26fb38344e4497ca6751bb10905c76bb689f4222b002204833806b014c26fd801727b792b1260003c55710f87c5adbd7a9cb57446dbc9801ffffffff0101000000000000002321037c615d761e71d38903609bf4f46847266edc2fb37532047d747ba47eaae5ffe1ac00000000\", \"P2SH\"],\n\n\n[\"OP_CODESEPARATOR tests\"],\n\n[\"Test that SignatureHash() removes OP_CODESEPARATOR with FindAndDelete()\"],\n[[[\"bc7fd132fcf817918334822ee6d9bd95c889099c96e07ca2c1eb2cc70db63224\", 0, \"CODESEPARATOR 0x21 0x038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041 CHECKSIG\"]],\n\"01000000012432b60dc72cebc1a27ce0969c0989c895bdd9e62e8234839117f8fc32d17fbc000000004a493046022100a576b52051962c25e642c0fd3d77ee6c92487048e5d90818bcf5b51abaccd7900221008204f8fb121be4ec3b24483b1f92d89b1b0548513a134e345c5442e86e8617a501ffffffff010000000000000000016a00000000\", \"P2SH\"],\n[[[\"83e194f90b6ef21fa2e3a365b63794fb5daa844bdc9b25de30899fcfe7b01047\", 0, \"CODESEPARATOR CODESEPARATOR 0x21 0x038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041 CHECKSIG\"]],\n\"01000000014710b0e7cf9f8930de259bdc4b84aa5dfb9437b665a3e3a21ff26e0bf994e183000000004a493046022100a166121a61b4eeb19d8f922b978ff6ab58ead8a5a5552bf9be73dc9c156873ea02210092ad9bc43ee647da4f6652c320800debcf08ec20a094a0aaf085f63ecb37a17201ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"Hashed data starts at the CODESEPARATOR\"],\n[[[\"326882a7f22b5191f1a0cc9962ca4b878cd969cf3b3a70887aece4d801a0ba5e\", 0, \"0x21 0x038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041 CODESEPARATOR CHECKSIG\"]],\n\"01000000015ebaa001d8e4ec7a88703a3bcf69d98c874bca6299cca0f191512bf2a7826832000000004948304502203bf754d1c6732fbf87c5dcd81258aefd30f2060d7bd8ac4a5696f7927091dad1022100f5bcb726c4cf5ed0ed34cc13dadeedf628ae1045b7cb34421bc60b89f4cecae701ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"But only if execution has reached it\"],\n[[[\"a955032f4d6b0c9bfe8cad8f00a8933790b9c1dc28c82e0f48e75b35da0e4944\", 0, \"0x21 0x038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041 CHECKSIGVERIFY CODESEPARATOR 0x21 0x038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041 CHECKSIGVERIFY CODESEPARATOR 1\"]],\n\"010000000144490eda355be7480f2ec828dcc1b9903793a8008fad8cfe9b0c6b4d2f0355a900000000924830450221009c0a27f886a1d8cb87f6f595fbc3163d28f7a81ec3c4b252ee7f3ac77fd13ffa02203caa8dfa09713c8c4d7ef575c75ed97812072405d932bd11e6a1593a98b679370148304502201e3861ef39a526406bad1e20ecad06be7375ad40ddb582c9be42d26c3a0d7b240221009d0a3985e96522e59635d19cc4448547477396ce0ef17a58e7d74c3ef464292301ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"CODESEPARATOR in an unexecuted IF block does not change what is hashed\"],\n[[[\"a955032f4d6b0c9bfe8cad8f00a8933790b9c1dc28c82e0f48e75b35da0e4944\", 0, \"IF CODESEPARATOR ENDIF 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 CHECKSIGVERIFY CODESEPARATOR 1\"]],\n\"010000000144490eda355be7480f2ec828dcc1b9903793a8008fad8cfe9b0c6b4d2f0355a9000000004a48304502207a6974a77c591fa13dff60cabbb85a0de9e025c09c65a4b2285e47ce8e22f761022100f0efaac9ff8ac36b10721e0aae1fb975c90500b50c56e8a0cc52b0403f0425dd0100ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"As above, with the IF block executed\"],\n[[[\"a955032f4d6b0c9bfe8cad8f00a8933790b9c1dc28c82e0f48e75b35da0e4944\", 0, \"IF CODESEPARATOR ENDIF 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 CHECKSIGVERIFY CODESEPARATOR 1\"]],\n\"010000000144490eda355be7480f2ec828dcc1b9903793a8008fad8cfe9b0c6b4d2f0355a9000000004a483045022100fa4a74ba9fd59c59f46c3960cf90cbe0d2b743c471d24a3d5d6db6002af5eebb02204d70ec490fd0f7055a7c45f86514336e3a7f03503dacecabb247fc23f15c83510151ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n\n[\"CHECKSIG is legal in scriptSigs\"],\n[[[\"ccf7f4053a02e653c36ac75c891b7496d0dc5ce5214f6c913d9cf8f1329ebee0\", 0, \"DUP HASH160 0x14 0xee5a6aa40facefb2655ac23c0c28c57c65c41f9b EQUALVERIFY CHECKSIG\"]],\n\"0100000001e0be9e32f1f89c3d916c4f21e55cdcd096741b895cc76ac353e6023a05f4f7cc00000000d86149304602210086e5f736a2c3622ebb62bd9d93d8e5d76508b98be922b97160edc3dcca6d8c47022100b23c312ac232a4473f19d2aeb95ab7bdf2b65518911a0d72d50e38b5dd31dc820121038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ac4730440220508fa761865c8abd81244a168392876ee1d94e8ed83897066b5e2df2400dad24022043f5ee7538e87e9c6aef7ef55133d3e51da7cc522830a9c4d736977a76ef755c0121038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"Same semantics for OP_CODESEPARATOR\"],\n[[[\"10c9f0effe83e97f80f067de2b11c6a00c3088a4bce42c5ae761519af9306f3c\", 1, \"DUP HASH160 0x14 0xee5a6aa40facefb2655ac23c0c28c57c65c41f9b EQUALVERIFY CHECKSIG\"]],\n\"01000000013c6f30f99a5161e75a2ce4bca488300ca0c6112bde67f0807fe983feeff0c91001000000e608646561646265656675ab61493046022100ce18d384221a731c993939015e3d1bcebafb16e8c0b5b5d14097ec8177ae6f28022100bcab227af90bab33c3fe0a9abfee03ba976ee25dc6ce542526e9b2e56e14b7f10121038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ac493046022100c3b93edcc0fd6250eb32f2dd8a0bba1754b0f6c3be8ed4100ed582f3db73eba2022100bf75b5bd2eff4d6bf2bda2e34a40fcc07d4aa3cf862ceaa77b47b81eff829f9a01ab21038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"Signatures are removed from the script they are in by FindAndDelete() in the CHECKSIG code; even multiple instances of one signature can be removed.\"],\n[[[\"6056ebd549003b10cbbd915cea0d82209fe40b8617104be917a26fa92cbe3d6f\", 0, \"DUP HASH160 0x14 0xee5a6aa40facefb2655ac23c0c28c57c65c41f9b EQUALVERIFY CHECKSIG\"]],\n\"01000000016f3dbe2ca96fa217e94b1017860be49f20820dea5c91bdcb103b0049d5eb566000000000fd1d0147304402203989ac8f9ad36b5d0919d97fa0a7f70c5272abee3b14477dc646288a8b976df5022027d19da84a066af9053ad3d1d7459d171b7e3a80bc6c4ef7a330677a6be548140147304402203989ac8f9ad36b5d0919d97fa0a7f70c5272abee3b14477dc646288a8b976df5022027d19da84a066af9053ad3d1d7459d171b7e3a80bc6c4ef7a330677a6be548140121038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ac47304402203757e937ba807e4a5da8534c17f9d121176056406a6465054bdd260457515c1a02200f02eccf1bec0f3a0d65df37889143c2e88ab7acec61a7b6f5aa264139141a2b0121038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"That also includes ahead of the opcode being executed.\"],\n[[[\"5a6b0021a6042a686b6b94abc36b387bef9109847774e8b1e51eb8cc55c53921\", 1, \"DUP HASH160 0x14 0xee5a6aa40facefb2655ac23c0c28c57c65c41f9b EQUALVERIFY CHECKSIG\"]],\n\"01000000012139c555ccb81ee5b1e87477840991ef7b386bc3ab946b6b682a04a621006b5a01000000fdb40148304502201723e692e5f409a7151db386291b63524c5eb2030df652b1f53022fd8207349f022100b90d9bbf2f3366ce176e5e780a00433da67d9e5c79312c6388312a296a5800390148304502201723e692e5f409a7151db386291b63524c5eb2030df652b1f53022fd8207349f022100b90d9bbf2f3366ce176e5e780a00433da67d9e5c79312c6388312a296a5800390121038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f2204148304502201723e692e5f409a7151db386291b63524c5eb2030df652b1f53022fd8207349f022100b90d9bbf2f3366ce176e5e780a00433da67d9e5c79312c6388312a296a5800390175ac4830450220646b72c35beeec51f4d5bc1cbae01863825750d7f490864af354e6ea4f625e9c022100f04b98432df3a9641719dbced53393022e7249fb59db993af1118539830aab870148304502201723e692e5f409a7151db386291b63524c5eb2030df652b1f53022fd8207349f022100b90d9bbf2f3366ce176e5e780a00433da67d9e5c79312c6388312a296a580039017521038479a0fa998cd35259a2ef0a7a5c68662c1474f88ccb6d08a7677bbec7f22041ffffffff010000000000000000016a00000000\", \"P2SH\"],\n\n[\"Finally CHECKMULTISIG removes all signatures prior to hashing the script containing those signatures. In conjunction with the SIGHASH_SINGLE bug this lets us test whether or not FindAndDelete() is actually present in scriptPubKey/redeemScript evaluation by including a signature of the digest 0x01 We can compute in advance for our pubkey, embed it in the scriptPubKey, and then also using a normal SIGHASH_ALL signature. If FindAndDelete() wasn't run, the 'bugged' signature would still be in the hashed script, and the normal signature would fail.\"],\n\n[\"Here's an example on mainnet within a P2SH redeemScript. Remarkably it's a standard transaction in <0.9\"],\n[[[\"b5b598de91787439afd5938116654e0b16b7a0d0f82742ba37564219c5afcbf9\", 0, \"DUP HASH160 0x14 0xf6f365c40f0739b61de827a44751e5e99032ed8f EQUALVERIFY CHECKSIG\"],\n  [\"ab9805c6d57d7070d9a42c5176e47bb705023e6b67249fb6760880548298e742\", 0, \"HASH160 0x14 0xd8dacdadb7462ae15cd906f1878706d0da8660e6 EQUAL\"]],\n\"0100000002f9cbafc519425637ba4227f8d0a0b7160b4e65168193d5af39747891de98b5b5000000006b4830450221008dd619c563e527c47d9bd53534a770b102e40faa87f61433580e04e271ef2f960220029886434e18122b53d5decd25f1f4acb2480659fea20aabd856987ba3c3907e0121022b78b756e2258af13779c1a1f37ea6800259716ca4b7f0b87610e0bf3ab52a01ffffffff42e7988254800876b69f24676b3e0205b77be476512ca4d970707dd5c60598ab00000000fd260100483045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a53034930460221008431bdfa72bc67f9d41fe72e94c88fb8f359ffa30b33c72c121c5a877d922e1002210089ef5fc22dd8bfc6bf9ffdb01a9862d27687d424d1fefbab9e9c7176844a187a014c9052483045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303210378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71210378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c7153aeffffffff01a08601000000000017a914d8dacdadb7462ae15cd906f1878706d0da8660e68700000000\", \"P2SH\"],\n\n[\"Same idea, but with bare CHECKMULTISIG\"],\n[[[\"ceafe58e0f6e7d67c0409fbbf673c84c166e3c5d3c24af58f7175b18df3bb3db\", 0, \"DUP HASH160 0x14 0xf6f365c40f0739b61de827a44751e5e99032ed8f EQUALVERIFY CHECKSIG\"],\n  [\"ceafe58e0f6e7d67c0409fbbf673c84c166e3c5d3c24af58f7175b18df3bb3db\", 1, \"2 0x48 0x3045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 3 CHECKMULTISIG\"]],\n\"0100000002dbb33bdf185b17f758af243c5d3c6e164cc873f6bb9f40c0677d6e0f8ee5afce000000006b4830450221009627444320dc5ef8d7f68f35010b4c050a6ed0d96b67a84db99fda9c9de58b1e02203e4b4aaa019e012e65d69b487fdf8719df72f488fa91506a80c49a33929f1fd50121022b78b756e2258af13779c1a1f37ea6800259716ca4b7f0b87610e0bf3ab52a01ffffffffdbb33bdf185b17f758af243c5d3c6e164cc873f6bb9f40c0677d6e0f8ee5afce010000009300483045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303483045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303ffffffff01a0860100000000001976a9149bc0bbdd3024da4d0c38ed1aecf5c68dd1d3fa1288ac00000000\", \"P2SH\"],\n\n\n[\"CHECKLOCKTIMEVERIFY tests\"],\n\n[\"By-height locks, with argument == 0 and == tx nLockTime\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0 CHECKLOCKTIMEVERIFY 1\"]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"499999999 CHECKLOCKTIMEVERIFY 1\"]],\n\"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ff64cd1d\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0 CHECKLOCKTIMEVERIFY 1\"]],\n\"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ff64cd1d\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"By-time locks, with argument just beyond tx nLockTime (but within numerical boundaries)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"500000000 CHECKLOCKTIMEVERIFY 1\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000065cd1d\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKLOCKTIMEVERIFY 1\"]],\n\"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ffffffff\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"500000000 CHECKLOCKTIMEVERIFY 1\"]],\n\"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ffffffff\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"Any non-maxint nSequence is fine\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0 CHECKLOCKTIMEVERIFY 1\"]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000feffffff0100000000000000000000000000\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"The argument can be calculated rather than created directly by a PUSHDATA\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"499999999 1ADD CHECKLOCKTIMEVERIFY 1\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000065cd1d\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"Perhaps even by an ADD producing a 5-byte result that is out of bounds for other opcodes\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483647 2147483647 ADD CHECKLOCKTIMEVERIFY 1\"]],\n\"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000feffffff\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"5 byte non-minimally-encoded arguments are valid\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x05 0x0000000000 CHECKLOCKTIMEVERIFY 1\"]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"Valid CHECKLOCKTIMEVERIFY in scriptSig\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"1\"]],\n\"01000000010001000000000000000000000000000000000000000000000000000000000000000000000251b1000000000100000000000000000001000000\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"Valid CHECKLOCKTIMEVERIFY in redeemScript\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0xc5b93064159b3b2d6ab506a41b1f50463771b988 EQUAL\"]],\n\"0100000001000100000000000000000000000000000000000000000000000000000000000000000000030251b1000000000100000000000000000001000000\", \"P2SH,CHECKLOCKTIMEVERIFY\"],\n\n[\"A transaction with a non-standard DER signature.\"],\n[[[\"b1dbc81696c8a9c0fccd0693ab66d7c368dbc38c0def4e800685560ddd1b2132\", 0, \"DUP HASH160 0x14 0x4b3bd7eba3bc0284fd3007be7f3be275e94f5826 EQUALVERIFY CHECKSIG\"]],\n\"010000000132211bdd0d568506804eef0d8cc3db68c3d766ab9306cdfcc0a9c89616c8dbb1000000006c493045022100c7bb0faea0522e74ff220c20c022d2cb6033f8d167fb89e75a50e237a35fd6d202203064713491b1f8ad5f79e623d0219ad32510bfaa1009ab30cbee77b59317d6e30001210237af13eb2d84e4545af287b919c2282019c9691cc509e78e196a9d8274ed1be0ffffffff0100000000000000001976a914f1b3ed2eda9a2ebe5a9374f692877cdf87c0f95b88ac00000000\", \"P2SH\"],\n\n[\"CHECKSEQUENCEVERIFY tests\"],\n\n[\"By-height locks, with argument == 0 and == txin.nSequence\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"65535 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffff00000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"65535 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffbf7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffbf7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"By-time locks, with argument == 0 and == txin.nSequence\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4194304 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4259839 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffff40000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4259839 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffff7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4194304 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffff7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n["
    val str2 =
      "\"Upper sequence with upper sequence is fine\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483648 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000800100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000800100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483648 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000feffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000feffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483648 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"Argument 2^31 with various nSequence\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483648 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffbf7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483648 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffff7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483648 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"Argument 2^32-1 with various nSequence\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffbf7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffff7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4294967295 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"Argument 3<<31 with various nSequence\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"6442450944 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffbf7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"6442450944 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffff7f0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"6442450944 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"5 byte non-minimally-encoded operandss are valid\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x05 0x0000000000 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"The argument can be calculated rather than created directly by a PUSHDATA\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4194303 1ADD CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"4194304 1SUB CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffff00000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"An ADD producing a 5-byte result that sets CTxIn::SEQUENCE_LOCKTIME_DISABLE_FLAG\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483647 65536 CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"2147483647 4259840 ADD CHECKSEQUENCEVERIFY 1\"]],\n\"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"Valid CHECKSEQUENCEVERIFY in scriptSig\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"1\"]],\n\"02000000010001000000000000000000000000000000000000000000000000000000000000000000000251b2010000000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"Valid CHECKSEQUENCEVERIFY in redeemScript\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0x7c17aff532f22beb54069942f9bf567a66133eaf EQUAL\"]],\n\"0200000001000100000000000000000000000000000000000000000000000000000000000000000000030251b2010000000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],[\"Valid CHECKSEQUENCEVERIFY even with negative tx version number\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0x7c17aff532f22beb54069942f9bf567a66133eaf EQUAL\"]],\n\"ffffffff01000100000000000000000000000000000000000000000000000000000000000000000000030251b2010000000100000000000000000000000000\", \"P2SH,CHECKSEQUENCEVERIFY\"],\n\n[\"Valid P2WPKH (Private key of segwit tests is L5AQtV2HDm4xGsseLokK2VAT2EtYKcTm3c7HwqnJBFt9LdaQULsM)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1000]],\n\"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e8030000000000001976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88ac02483045022100cfb07164b36ba64c1b1e8c7720a56ad64d96f6ef332d3d37f9cb3c96477dc44502200a464cd7a9cf94cd70f66ce4f4f0625ef650052c7afcfe29d7d7e01830ff91ed012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7100000000\", \"P2SH,WITNESS\"],\n\n[\"Valid P2WSH\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x00 0x20 0xff25429251b5a84f452230a3c75fd886b7fc5a7865ce4a7bb7a9d7c5be6da3db\", 1000]],\n\"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e8030000000000001976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88ac02483045022100aa5d8aa40a90f23ce2c3d11bc845ca4a12acd99cbea37de6b9f6d86edebba8cb022022dedc2aa0a255f74d04c0b76ece2d7c691f9dd11a64a8ac49f62a99c3a05f9d01232103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ac00000000\", \"P2SH,WITNESS\"],\n\n[\"Valid P2SH(P2WPKH)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0xfe9c7dacc9fcfbf7e3b7d5ad06aa2b28c5a7b7e3 EQUAL\", 1000]],\n\"01000000000101000100000000000000000000000000000000000000000000000000000000000000000000171600144c9c3dfac4207d5d8cb89df5722cb3d712385e3fffffffff01e8030000000000001976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88ac02483045022100cfb07164b36ba64c1b1e8c7720a56ad64d96f6ef332d3d37f9cb3c96477dc44502200a464cd7a9cf94cd70f66ce4f4f0625ef650052c7afcfe29d7d7e01830ff91ed012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7100000000\", \"P2SH,WITNESS\"],\n\n[\"Valid P2SH(P2WSH)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"HASH160 0x14 0x2135ab4f0981830311e35600eebc7376dce3a914 EQUAL\", 1000]],\n\"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000023220020ff25429251b5a84f452230a3c75fd886b7fc5a7865ce4a7bb7a9d7c5be6da3dbffffffff01e8030000000000001976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88ac02483045022100aa5d8aa40a90f23ce2c3d11bc845ca4a12acd99cbea37de6b9f6d86edebba8cb022022dedc2aa0a255f74d04c0b76ece2d7c691f9dd11a64a8ac49f62a99c3a05f9d01232103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ac00000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash Single|AnyoneCanPay\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3100],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1100],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 3, \"0x51\", 4100]],\n\"0100000000010400010000000000000000000000000000000000000000000000000000000000000200000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000300000000ffffffff05540b0000000000000151d0070000000000000151840300000000000001513c0f00000000000001512c010000000000000151000248304502210092f4777a0f17bf5aeb8ae768dec5f2c14feabf9d1fe2c89c78dfed0f13fdb86902206da90a86042e252bcd1e80a168c719e4a1ddcc3cebea24b9812c5453c79107e9832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71000000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash Single|AnyoneCanPay (same signature as previous)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b0000000000000151000248304502210092f4777a0f17bf5aeb8ae768dec5f2c14feabf9d1fe2c89c78dfed0f13fdb86902206da90a86042e252bcd1e80a168c719e4a1ddcc3cebea24b9812c5453c79107e9832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash Single\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff0484030000000000000151d0070000000000000151540b0000000000000151c800000000000000015100024730440220699e6b0cfe015b64ca3283e6551440a34f901ba62dd4c72fe1cb815afb2e6761022021cc5e84db498b1479de14efda49093219441adc6c543e5534979605e273d80b032103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash Single (same signature as previous)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b000000000000015100024730440220699e6b0cfe015b64ca3283e6551440a34f901ba62dd4c72fe1cb815afb2e6761022021cc5e84db498b1479de14efda49093219441adc6c543e5534979605e273d80b032103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash None|AnyoneCanPay\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3100],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1100],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 3, \"0x51\", 4100]],\n\"0100000000010400010000000000000000000000000000000000000000000000000000000000000200000000ffffffff00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000300000000ffffffff04b60300000000000001519e070000000000000151860b00000000000001009600000000000000015100000248304502210091b32274295c2a3fa02f5bce92fb2789e3fc6ea947fbe1a76e52ea3f4ef2381a022079ad72aefa3837a2e0c033a8652a59731da05fa4a813f4fc48e87c075037256b822103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash None|AnyoneCanPay (same signature as previous)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b0000000000000151000248304502210091b32274295c2a3fa02f5bce92fb2789e3fc6ea947fbe1a76e52ea3f4ef2381a022079ad72aefa3837a2e0c033a8652a59731da05fa4a813f4fc48e87c075037256b822103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash None\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff04b60300000000000001519e070000000000000151860b0000000000000100960000000000000001510002473044022022fceb54f62f8feea77faac7083c3b56c4676a78f93745adc8a35800bc36adfa022026927df9abcf0a8777829bcfcce3ff0a385fa54c3f9df577405e3ef24ee56479022103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash None (same signature as previous)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b00000000000001510002473044022022fceb54f62f8feea77faac7083c3b56c4676a78f93745adc8a35800bc36adfa022026927df9abcf0a8777829bcfcce3ff0a385fa54c3f9df577405e3ef24ee56479022103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash None (same signature, only sequences changed)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"01000000000103000100000000000000000000000000000000000000000000000000000000000000000000000200000000010000000000000000000000000000000000000000000000000000000000000100000000ffffffff000100000000000000000000000000000000000000000000000000000000000002000000000200000003e8030000000000000151d0070000000000000151b80b00000000000001510002473044022022fceb54f62f8feea77faac7083c3b56c4676a78f93745adc8a35800bc36adfa022026927df9abcf0a8777829bcfcce3ff0a385fa54c3f9df577405e3ef24ee56479022103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash All|AnyoneCanPay\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3100],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1100],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 3, \"0x51\", 4100]],\n\"0100000000010400010000000000000000000000000000000000000000000000000000000000000200000000ffffffff00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000300000000ffffffff03e8030000000000000151d0070000000000000151b80b0000000000000151000002483045022100a3cec69b52cba2d2de623eeef89e0ba1606184ea55476c0f8189fda231bc9cbb022003181ad597f7c380a7d1c740286b1d022b8b04ded028b833282e055e03b8efef812103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with SigHash All|AnyoneCanPay (same signature as previous)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b00000000000001510002483045022100a3cec69b52cba2d2de623eeef89e0ba1606184ea55476c0f8189fda231bc9cbb022003181ad597f7c380a7d1c740286b1d022b8b04ded028b833282e055e03b8efef812103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Unknown witness program version  (without DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x60 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 2000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"0x51\", 3000]],\n\"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b00000000000001510002483045022100a3cec69b52cba2d2de623ffffffffff1606184ea55476c0f8189fda231bc9cbb022003181ad597f7c380a7d1c740286b1d022b8b04ded028b833282e055e03b8efef812103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Witness with a push of 520 bytes\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x00 0x20 0x33198a9bfef674ebddb9ffaa52928017b8472791e54c609cb95f278ac6b1e349\", 1000]],\n\"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff010000000000000000015102fd08020000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002755100000000\", \"P2SH,WITNESS\"],\n\n[\"Transaction mixing all SigHash, segwit and normal inputs\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1001],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 2, \"DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG\", 1002],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 3, \"DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG\", 1003],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 4, \"DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG\", 1004],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 5, \"DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG\", 1005],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 6, \"DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG\", 1006],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 7, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1007],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 8, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1008],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 9, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1009],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 10, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1010],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 11, \"DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG\", 1011]],\n\"0100000000010c00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff0001000000000000000000000000000000000000000000000000000000000000020000006a473044022026c2e65b33fcd03b2a3b0f25030f0244bd23cc45ae4dec0f48ae62255b1998a00220463aa3982b718d593a6b9e0044513fd67a5009c2fdccc59992cffc2b167889f4012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000030000006a4730440220008bd8382911218dcb4c9f2e75bf5c5c3635f2f2df49b36994fde85b0be21a1a02205a539ef10fb4c778b522c1be852352ea06c67ab74200977c722b0bc68972575a012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000040000006b483045022100d9436c32ff065127d71e1a20e319e4fe0a103ba0272743dbd8580be4659ab5d302203fd62571ee1fe790b182d078ecfd092a509eac112bea558d122974ef9cc012c7012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000050000006a47304402200e2c149b114ec546015c13b2b464bbcb0cdc5872e6775787527af6cbc4830b6c02207e9396c6979fb15a9a2b96ca08a633866eaf20dc0ff3c03e512c1d5a1654f148012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000060000006b483045022100b20e70d897dc15420bccb5e0d3e208d27bdd676af109abbd3f88dbdb7721e6d6022005836e663173fbdfe069f54cde3c2decd3d0ea84378092a5d9d85ec8642e8a41012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff00010000000000000000000000000000000000000000000000000000000000000700000000ffffffff00010000000000000000000000000000000000000000000000000000000000000800000000ffffffff00010000000000000000000000000000000000000000000000000000000000000900000000ffffffff00010000000000000000000000000000000000000000000000000000000000000a00000000ffffffff00010000000000000000000000000000000000000000000000000000000000000b0000006a47304402206639c6e05e3b9d2675a7f3876286bdf7584fe2bbd15e0ce52dd4e02c0092cdc60220757d60b0a61fc95ada79d23746744c72bac1545a75ff6c2c7cdb6ae04e7e9592012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0ce8030000000000000151e9030000000000000151ea030000000000000151eb030000000000000151ec030000000000000151ed030000000000000151ee030000000000000151ef030000000000000151f0030000000000000151f1030000000000000151f2030000000000000151f30300000000000001510248304502210082219a54f61bf126bfc3fa068c6e33831222d1d7138c6faa9d33ca87fd4202d6022063f9902519624254d7c2c8ea7ba2d66ae975e4e229ae38043973ec707d5d4a83012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7102473044022017fb58502475848c1b09f162cb1688d0920ff7f142bed0ef904da2ccc88b168f02201798afa61850c65e77889cbcd648a5703b487895517c88f85cdd18b021ee246a012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7100000000000247304402202830b7926e488da75782c81a54cd281720890d1af064629ebf2e31bf9f5435f30220089afaa8b455bbeb7d9b9c3fe1ed37d07685ade8455c76472cda424d93e4074a012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7102473044022026326fcdae9207b596c2b05921dbac11d81040c4d40378513670f19d9f4af893022034ecd7a282c0163b89aaa62c22ec202cef4736c58cd251649bad0d8139bcbf55012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71024730440220214978daeb2f38cd426ee6e2f44131a33d6b191af1c216247f1dd7d74c16d84a02205fdc05529b0bc0c430b4d5987264d9d075351c4f4484c16e91662e90a72aab24012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710247304402204a6e9f199dc9672cf2ff8094aaa784363be1eb62b679f7ff2df361124f1dca3302205eeb11f70fab5355c9c8ad1a0700ea355d315e334822fa182227e9815308ee8f012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000\", \"P2SH,WITNESS\"],\n\n[\"Unknown version witness program with empty witness\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x60 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1000]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e803000000000000015100000000\", \"P2SH,WITNESS\"],\n\n[\"Witness SIGHASH_SINGLE with output out of bound\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x51\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x20 0x4d6c2a32c87821d68fc016fca70797abdb80df6cd84651d40a9300c6bad79e62\", 1000]],\n\"0100000000010200010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff01d00700000000000001510003483045022100e078de4e96a0e05dcdc0a414124dd8475782b5f3f0ed3f607919e9a5eeeb22bf02201de309b3a3109adb3de8074b3610d4cf454c49b61247a2779a0bcbf31c889333032103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc711976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88ac00000000\", \"P2SH,WITNESS\"],\n\n[\"1 byte push should not be considered a witness scriptPubKey\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x60 0x01 0x01\", 1000]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e803000000000000015100000000\", \"P2SH,WITNESS,DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM\"],\n\n[\"41 bytes push should not be considered a witness scriptPubKey\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x60 0x29 0xff25429251b5a84f452230a3c75fd886b7fc5a7865ce4a7bb7a9d7c5be6da3dbff0000000000000000\", 1000]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e803000000000000015100000000\", \"P2SH,WITNESS,DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM\"],\n\n[\"The witness version must use OP_1 to OP_16 only\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x01 0x10 0x02 0x0001\", 1000]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e803000000000000015100000000\", \"P2SH,WITNESS,DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM\"],\n\n[\"The witness program push must be canonical\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x60 0x4c02 0x0001\", 1000]],\n\"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e803000000000000015100000000\", \"P2SH,WITNESS,DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM\"],\n\n[\"Witness Single|AnyoneCanPay does not hash input's position\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1001]],\n\"0100000000010200010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff02e8030000000000000151e90300000000000001510247304402206d59682663faab5e4cb733c562e22cdae59294895929ec38d7c016621ff90da0022063ef0af5f970afe8a45ea836e3509b8847ed39463253106ac17d19c437d3d56b832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710248304502210085001a820bfcbc9f9de0298af714493f8a37b3b354bfd21a7097c3e009f2018c022050a8b4dbc8155d4d04da2f5cdd575dcf8dd0108de8bec759bd897ea01ecb3af7832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7100000000\", \"P2SH,WITNESS\"],\n\n[\"Witness Single|AnyoneCanPay does not hash input's position (permutation)\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1001],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f\", 1000]],\n\"0100000000010200010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff02e9030000000000000151e80300000000000001510248304502210085001a820bfcbc9f9de0298af714493f8a37b3b354bfd21a7097c3e009f2018c022050a8b4dbc8155d4d04da2f5cdd575dcf8dd0108de8bec759bd897ea01ecb3af7832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710247304402206d59682663faab5e4cb733c562e22cdae59294895929ec38d7c016621ff90da0022063ef0af5f970afe8a45ea836e3509b8847ed39463253106ac17d19c437d3d56b832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7100000000\", \"P2SH,WITNESS\"],\n\n[\"Non witness Single|AnyoneCanPay hash input's position\"],\n[[[\"0000000000000000000000000000000000000000000000000000000000000100\", 0, \"0x21 0x03596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71 CHECKSIG\", 1000],\n[\"0000000000000000000000000000000000000000000000000000000000000100\", 1, \"0x21 0x03596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71 CHECKSIG\", 1001]],\n\"01000000020001000000000000000000000000000000000000000000000000000000000000000000004847304402202a0b4b1294d70540235ae033d78e64b4897ec859c7b6f1b2b1d8a02e1d46006702201445e756d2254b0f1dfda9ab8e1e1bc26df9668077403204f32d16a49a36eb6983ffffffff00010000000000000000000000000000000000000000000000000000000000000100000049483045022100acb96cfdbda6dc94b489fd06f2d720983b5f350e31ba906cdbd800773e80b21c02200d74ea5bdf114212b4bbe9ed82c36d2e369e302dff57cb60d01c428f0bd3daab83ffffffff02e8030000000000000151e903000000000000015100000000\", \"P2SH,WITNESS\"],\n\n[\"BIP143 examples: details and private keys are available in BIP143\"],\n[\"BIP143 example: P2WSH with OP_CODESEPARATOR and out-of-range SIGHASH_SINGLE.\"],\n[[[\"6eb316926b1c5d567cd6f5e6a84fec606fc53d7b474526d1fff3948020c93dfe\", 0, \"0x21 0x036d5c20fa14fb2f635474c1dc4ef5909d4568e5569b79fc94d3448486e14685f8 CHECKSIG\", 156250000],\n[\"f825690aee1b3dc247da796cacb12687a5e802429fd291cfd63e010f02cf1508\", 0, \"0x00 0x20 0x5d1b56b63d714eebe542309525f484b7e9d6f686b3781b6f61ef925d66d6f6a0\", 4900000000]],\n\"01000000000102fe3dc9208094f3ffd12645477b3dc56f60ec4fa8e6f5d67c565d1c6b9216b36e000000004847304402200af4e47c9b9629dbecc21f73af989bdaa911f7e6f6c2e9394588a3aa68f81e9902204f3fcf6ade7e5abb1295b6774c8e0abd94ae62217367096bc02ee5e435b67da201ffffffff0815cf020f013ed6cf91d29f4202e8a58726b1ac6c79da47c23d1bee0a6925f80000000000ffffffff0100f2052a010000001976a914a30741f8145e5acadf23f751864167f32e0963f788ac000347304402200de66acf4527789bfda55fc5459e214fa6083f936b430a762c629656216805ac0220396f550692cd347171cbc1ef1f51e15282e837bb2b30860dc77c8f78bc8501e503473044022027dc95ad6b740fe5129e7e62a75dd00f291a2aeb1200b84b09d9e3789406b6c002201a9ecd315dd6a0e632ab20bbb98948bc0c6fb204f2c286963bb48517a7058e27034721026dccc749adc2a9d0d89497ac511f760f45c47dc5ed9cf352a58ac706453880aeadab210255a9626aebf5e29c0e6538428ba0d1dcf6ca98ffdf086aa8ced5e0d0215ea465ac00000000\", \"P2SH,WITNESS\"],\n\n[\"BIP143 example: P2WSH with unexecuted OP_CODESEPARATOR and SINGLE|ANYONECANPAY\"],\n[[[\"01c0cf7fba650638e55eb91261b183251fbb466f90dff17f10086817c542b5e9\", 0, \"0x00 0x20 0xba468eea561b26301e4cf69fa34bde4ad60c81e70f059f045ca9a79931004a4d\", 16777215],\n[\"1b2a9a426ba603ba357ce7773cb5805cb9c7c2b386d100d1fc9263513188e680\", 0, \"0x00 0x20 0xd9bbfbe56af7c4b7f960a70d7ea107156913d9e5a26b0a71429df5e097ca6537\", 16777215]],\n\"01000000000102e9b542c5176808107ff1df906f46bb1f2583b16112b95ee5380665ba7fcfc0010000000000ffffffff80e68831516392fcd100d186b3c2c7b95c80b53c77e77c35ba03a66b429a2a1b0000000000ffffffff0280969800000000001976a914de4b231626ef508c9a74a8517e6783c0546d6b2888ac80969800000000001976a9146648a8cd4531e1ec47f35916de8e259237294d1e88ac02483045022100f6a10b8604e6dc910194b79ccfc93e1bc0ec7c03453caaa8987f7d6c3413566002206216229ede9b4d6ec2d325be245c5b508ff0339bf1794078e20bfe0babc7ffe683270063ab68210392972e2eb617b2388771abe27235fd5ac44af8e61693261550447a4c3e39da98ac024730440220032521802a76ad7bf74d0e2c218b72cf0cbc867066e2e53db905ba37f130397e02207709e2188ed7f08f4c952d9d13986da504502b8c3be59617e043552f506c46ff83275163ab68210392972e2eb617b2388771abe27235fd5ac44af8e61693261550447a4c3e39da98ac00000000\", \"P2SH,WITNESS\"],\n\n[\"BIP143 example: Same as the previous example with input-output pairs swapped\"],\n[[[\"1b2a9a426ba603ba357ce7773cb5805cb9c7c2b386d100d1fc9263513188e680\", 0, \"0x00 0x20 0xd9bbfbe56af7c4b7f960a70d7ea107156913d9e5a26b0a71429df5e097ca6537\", 16777215],\n[\"01c0cf7fba650638e55eb91261b183251fbb466f90dff17f10086817c542b5e9\", 0, \"0x00 0x20 0xba468eea561b26301e4cf69fa34bde4ad60c81e70f059f045ca9a79931004a4d\", 16777215]],\n\"0100000000010280e68831516392fcd100d186b3c2c7b95c80b53c77e77c35ba03a66b429a2a1b0000000000ffffffffe9b542c5176808107ff1df906f46bb1f2583b16112b95ee5380665ba7fcfc0010000000000ffffffff0280969800000000001976a9146648a8cd4531e1ec47f35916de8e259237294d1e88ac80969800000000001976a914de4b231626ef508c9a74a8517e6783c0546d6b2888ac024730440220032521802a76ad7bf74d0e2c218b72cf0cbc867066e2e53db905ba37f130397e02207709e2188ed7f08f4c952d9d13986da504502b8c3be59617e043552f506c46ff83275163ab68210392972e2eb617b2388771abe27235fd5ac44af8e61693261550447a4c3e39da98ac02483045022100f6a10b8604e6dc910194b79ccfc93e1bc0ec7c03453caaa8987f7d6c3413566002206216229ede9b4d6ec2d325be245c5b508ff0339bf1794078e20bfe0babc7ffe683270063ab68210392972e2eb617b2388771abe27235fd5ac44af8e61693261550447a4c3e39da98ac00000000\", \"P2SH,WITNESS\"],\n\n[\"BIP143 example: P2SH-P2WSH 6-of-6 multisig signed with 6 different SIGHASH types\"],\n[[[\"6eb98797a21c6c10aa74edf29d618be109f48a8e94c694f3701e08ca69186436\", 1, \"HASH160 0x14 0x9993a429037b5d912407a71c252019287b8d27a5 EQUAL\", 987654321]],\n\"0100000000010136641869ca081e70f394c6948e8af409e18b619df2ed74aa106c1ca29787b96e0100000023220020a16b5755f7f6f96dbd65f5f0d6ab9418b89af4b1f14a1bb8a09062c35f0dcb54ffffffff0200e9a435000000001976a914389ffce9cd9ae88dcc0631e88a821ffdbe9bfe2688acc0832f05000000001976a9147480a33f950689af511e6e84c138dbbd3c3ee41588ac080047304402206ac44d672dac41f9b00e28f4df20c52eeb087207e8d758d76d92c6fab3b73e2b0220367750dbbe19290069cba53d096f44530e4f98acaa594810388cf7409a1870ce01473044022068c7946a43232757cbdf9176f009a928e1cd9a1a8c212f15c1e11ac9f2925d9002205b75f937ff2f9f3c1246e547e54f62e027f64eefa2695578cc6432cdabce271502473044022059ebf56d98010a932cf8ecfec54c48e6139ed6adb0728c09cbe1e4fa0915302e022007cd986c8fa870ff5d2b3a89139c9fe7e499259875357e20fcbb15571c76795403483045022100fbefd94bd0a488d50b79102b5dad4ab6ced30c4069f1eaa69a4b5a763414067e02203156c6a5c9cf88f91265f5a942e96213afae16d83321c8b31bb342142a14d16381483045022100a5263ea0553ba89221984bd7f0b13613db16e7a70c549a86de0cc0444141a407022005c360ef0ae5a5d4f9f2f87a56c1546cc8268cab08c73501d6b3be2e1e1a8a08824730440220525406a1482936d5a21888260dc165497a90a15669636d8edca6b9fe490d309c022032af0c646a34a44d1f4576bf6a4a74b67940f8faa84c7df9abe12a01a11e2b4783cf56210307b8ae49ac90a048e9b53357a2354b3334e9c8bee813ecb98e99a7e07e8c3ba32103b28f0c28bfab54554ae8c658ac5c3e0ce6e79ad336331f78c428dd43eea8449b21034b8113d703413d57761b8b9781957b8c0ac1dfe69f492580ca4195f50376ba4a21033400f6afecb833092a9a21cfdf1ed1376e58c5d1f47de74683123987e967a8f42103a6d48b1131e94ba04d9737d61acdaa1322008af9602b3b14862c07a1789aac162102d8b661b0b3302ee2f162b09e07a55ad5dfbe673a9f01d9f0c19617681024306b56ae00000000\", \"P2SH,WITNESS\"],\n\n[\"FindAndDelete tests\"],\n[\"This is a test of FindAndDelete. The first tx is a spend of normal P2SH and the second tx is a spend of bare P2WSH.\"],\n[\"The redeemScript/witnessScript is CHECKSIGVERIFY <0x30450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01>.\"],\n[\"The signature is <0x30450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01> <pubkey>,\"],\n[\"where the pubkey is obtained through key recovery with sig and correct sighash.\"],\n[\"This is to show that FindAndDelete is applied only to non-segwit scripts\"],\n[\"Non-segwit: correct sighash (with FindAndDelete) = 1ba1fe3bc90c5d1265460e684ce6774e324f0fabdf67619eda729e64e8b6bc08\"],\n[[[\"f18783ace138abac5d3a7a5cf08e88fe6912f267ef936452e0c27d090621c169\", 7000, \"HASH160 0x14 0x0c746489e2d83cdbb5b90b432773342ba809c134 EQUAL\", 200000]],\n\"010000000169c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f1581b0000b64830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0121037a3fb04bcdb09eba90f69961ba1692a3528e45e67c85b200df820212d7594d334aad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01ffffffff0101000000000000000000000000\", \"P2SH,WITNESS\"],\n[\"BIP143: correct sighash (without FindAndDelete) = 71c9cd9b2869b9c70b01b1f0360c148f42dee72297db312638df136f43311f23\"],\n[[[\"f18783ace138abac5d3a7a5cf08e88fe6912f267ef936452e0c27d090621c169\", 7500, \"0x00 0x20 0x9e1be07558ea5cc8e02ed1d80c0911048afad949affa36d5c3951e3159dbea19\", 200000]],\n\"0100000000010169c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f14c1d000000ffffffff01010000000000000000034830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e012102a9781d66b61fb5a7ef00ac5ad5bc6ffc78be7b44a566e3c87870e1079368df4c4aad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0100000000\", \"P2SH,WITNESS\"],\n[\"This is multisig version of the FindAndDelete tests\"],\n[\"Script is 2 CHECKMULTISIGVERIFY <sig1> <sig2> DROP\"],\n[\"52af4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c0395960175\"],\n[\"Signature is 0 <sig1> <sig2> 2 <key1> <key2>\"],\n[\"Non-segwit: correct sighash (with FindAndDelete) = 1d50f00ba4db2917b903b0ec5002e017343bb38876398c9510570f5dce099295\"],\n[[[\"9628667ad48219a169b41b020800162287d2c0f713c04157e95c484a8dcb7592\", 7000, \"HASH160 0x14 0x5748407f5ca5cdca53ba30b79040260770c9ee1b EQUAL\", 200000]],\n\"01000000019275cb8d4a485ce95741c013f7c0d28722160008021bb469a11982d47a662896581b0000fd6f01004830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c03959601522102cd74a2809ffeeed0092bc124fd79836706e41f048db3f6ae9df8708cefb83a1c2102e615999372426e46fd107b76eaf007156a507584aa2cc21de9eee3bdbd26d36c4c9552af4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c0395960175ffffffff0101000000000000000000000000\", \"P2SH,WITNESS\"],\n[\"BIP143: correct sighash (without FindAndDelete) = c1628a1e7c67f14ca0c27c06e4fdeec2e6d1a73c7a91d7c046ff83e835aebb72\"],\n[[[\"9628667ad48219a169b41b020800162287d2c0f713c04157e95c484a8dcb7592\", 7500, \"0x00 0x20 0x9b66c15b4e0b4eb49fa877982cafded24859fe5b0e2dbfbe4f0df1de7743fd52\", 200000]],\n\"010000000001019275cb8d4a485ce95741c013f7c0d28722160008021bb469a11982d47a6628964c1d000000ffffffff0101000000000000000007004830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c0395960101022102966f109c54e85d3aee8321301136cedeb9fc710fdef58a9de8a73942f8e567c021034ffc99dd9a79dd3cb31e2ab3e0b09e0e67db41ac068c625cd1f491576016c84e9552af4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c039596017500000000\", \"P2SH,WITNESS\"],\n\n[\"Make diffs cleaner by leaving a comment here without comma at the end\"]\n]"
    str1 ++ str2
  }

  val invalid: String =
    """[
      |["The following are deserialized transactions which are invalid."],
      |["They are in the form"],
      |["[[[prevout hash, prevout index, prevout scriptPubKey, amount?], [input 2], ...],"],
      |["serializedTransaction, verifyFlags]"],
      |["Objects that are only a single string (like this one) are ignored"],
      |
      |["0e1b5688cf179cd9f7cbda1fac0090f6e684bbf8cd946660120197c3f3681809 but with extra junk appended to the end of the scriptPubKey"],
      |[[["6ca7ec7b1847f6bdbd737176050e6a08d66ccd55bb94ad24f4018024107a5827", 0, "0x41 0x043b640e983c9690a14c039a2037ecc3467b27a0dcd58f19d76c7bc118d09fec45adc5370a1c5bf8067ca9f5557a4cf885fdb0fe0dcc9c3a7137226106fbc779a5 CHECKSIG VERIFY 1"]],
      |"010000000127587a10248001f424ad94bb55cd6cd6086a0e05767173bdbdf647187beca76c000000004948304502201b822ad10d6adc1a341ae8835be3f70a25201bbff31f59cbb9c5353a5f0eca18022100ea7b2f7074e9aa9cf70aa8d0ffee13e6b45dddabf1ab961bda378bcdb778fa4701ffffffff0100f2052a010000001976a914fc50c5907d86fed474ba5ce8b12a66e0a4c139d888ac00000000", "P2SH"],
      |
      |["This is the nearly-standard transaction with CHECKSIGVERIFY 1 instead of CHECKSIG from tx_valid.json"],
      |["but with the signature duplicated in the scriptPubKey with a non-standard pushdata prefix"],
      |["See FindAndDelete, which will only remove if it uses the same pushdata prefix as is standard"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "DUP HASH160 0x14 0x5b6462475454710f3c22f5fdf0b40704c92f25c3 EQUALVERIFY CHECKSIGVERIFY 1 0x4c 0x47 0x3044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a01"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006a473044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a012103ba8c8b86dea131c22ab967e6dd99bdae8eff7a1f75a2c35f1f944109e3fe5e22ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["Same as above, but with the sig in the scriptSig also pushed with the same non-standard OP_PUSHDATA"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "DUP HASH160 0x14 0x5b6462475454710f3c22f5fdf0b40704c92f25c3 EQUALVERIFY CHECKSIGVERIFY 1 0x4c 0x47 0x3044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a01"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006b4c473044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a012103ba8c8b86dea131c22ab967e6dd99bdae8eff7a1f75a2c35f1f944109e3fe5e22ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["This is the nearly-standard transaction with CHECKSIGVERIFY 1 instead of CHECKSIG from tx_valid.json"],
      |["but with the signature duplicated in the scriptPubKey with a different hashtype suffix"],
      |["See FindAndDelete, which will only remove if the signature, including the hash type, matches"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "DUP HASH160 0x14 0x5b6462475454710f3c22f5fdf0b40704c92f25c3 EQUALVERIFY CHECKSIGVERIFY 1 0x47 0x3044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a81"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006a473044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a012103ba8c8b86dea131c22ab967e6dd99bdae8eff7a1f75a2c35f1f944109e3fe5e22ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["An invalid P2SH Transaction"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0x7a052c840ba73af26755de42cf01cc9e0a49fef0 EQUAL"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000009085768617420697320ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["Tests for CheckTransaction()"],
      |["No outputs"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0x05ab9e14d983742513f0f451e105ffb4198d1dd4 EQUAL"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006d483045022100f16703104aab4e4088317c862daec83440242411b039d14280e03dd33b487ab802201318a7be236672c5c56083eb7a5a195bc57a40af7923ff8545016cd3b571e2a601232103c40e5d339df3f30bf753e7e04450ae4ef76c9e45587d1d993bdc4cd06f0651c7acffffffff0000000000", "P2SH"],
      |
      |["Negative output"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0xae609aca8061d77c5e111f6bb62501a6bbe2bfdb EQUAL"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006d4830450220063222cbb128731fc09de0d7323746539166544d6c1df84d867ccea84bcc8903022100bf568e8552844de664cd41648a031554327aa8844af34b4f27397c65b92c04de0123210243ec37dee0e2e053a9c976f43147e79bc7d9dc606ea51010af1ac80db6b069e1acffffffff01ffffffffffffffff015100000000", "P2SH"],
      |
      |["MAX_MONEY + 1 output"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0x32afac281462b822adbec5094b8d4d337dd5bd6a EQUAL"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006e493046022100e1eadba00d9296c743cb6ecc703fd9ddc9b3cd12906176a226ae4c18d6b00796022100a71aef7d2874deff681ba6080f1b278bac7bb99c61b08a85f4311970ffe7f63f012321030c0588dc44d92bdcbf8e72093466766fdc265ead8db64517b0c542275b70fffbacffffffff010140075af0750700015100000000", "P2SH"],
      |
      |["MAX_MONEY output + 1 output"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0xb558cbf4930954aa6a344363a15668d7477ae716 EQUAL"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000006d483045022027deccc14aa6668e78a8c9da3484fbcd4f9dcc9bb7d1b85146314b21b9ae4d86022100d0b43dece8cfb07348de0ca8bc5b86276fa88f7f2138381128b7c36ab2e42264012321029bb13463ddd5d2cc05da6e84e37536cb9525703cfd8f43afdb414988987a92f6acffffffff020040075af075070001510001000000000000015100000000", "P2SH"],
      |
      |["Duplicate inputs"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0x236d0639db62b0773fd8ac34dc85ae19e9aba80a EQUAL"]],
      |"01000000020001000000000000000000000000000000000000000000000000000000000000000000006c47304402204bb1197053d0d7799bf1b30cd503c44b58d6240cccbdc85b6fe76d087980208f02204beeed78200178ffc6c74237bb74b3f276bbb4098b5605d814304fe128bf1431012321039e8815e15952a7c3fada1905f8cf55419837133bd7756c0ef14fc8dfe50c0deaacffffffff0001000000000000000000000000000000000000000000000000000000000000000000006c47304402202306489afef52a6f62e90bf750bbcdf40c06f5c6b138286e6b6b86176bb9341802200dba98486ea68380f47ebb19a7df173b99e6bc9c681d6ccf3bde31465d1f16b3012321039e8815e15952a7c3fada1905f8cf55419837133bd7756c0ef14fc8dfe50c0deaacffffffff010000000000000000015100000000", "P2SH"],
      |
      |["Coinbase of size 1"],
      |["Note the input is just required to make the tester happy"],
      |[[["0000000000000000000000000000000000000000000000000000000000000000", -1, "1"]],
      |"01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0151ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["Coinbase of size 101"],
      |["Note the input is just required to make the tester happy"],
      |[[["0000000000000000000000000000000000000000000000000000000000000000", -1, "1"]],
      |"01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff655151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151515151ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["Null txin, but without being a coinbase (because there are two inputs)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000000", -1, "1"],
      |  ["0000000000000000000000000000000000000000000000000000000000000100", 0, "1"]],
      |"01000000020000000000000000000000000000000000000000000000000000000000000000ffffffff00ffffffff00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff010000000000000000015100000000", "P2SH"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1"],
      |  ["0000000000000000000000000000000000000000000000000000000000000000", -1, "1"]],
      |"010000000200010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0000000000000000000000000000000000000000000000000000000000000000ffffffff00ffffffff010000000000000000015100000000", "P2SH"],
      |
      |["Same as the transactions in valid with one input SIGHASH_ALL and one SIGHASH_ANYONECANPAY, but we set the _ANYONECANPAY sequence number, invalidating the SIGHASH_ALL signature"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG"],
      |  ["0000000000000000000000000000000000000000000000000000000000000200", 0, "0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG"]],
      | "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df10101000000000200000000000000000000000000000000000000000000000000000000000000000000484730440220201dc2d030e380e8f9cfb41b442d930fa5a685bb2c8db5906671f865507d0670022018d9e7a8d4c8d86a73c2a724ee38ef983ec249827e0e464841735955c707ece98101000000010100000000000000015100000000", "P2SH"],
      |
      |["CHECKMULTISIG with incorrect signature order"],
      |["Note the input is just required to make the tester happy"],
      |[[["b3da01dd4aae683c7aee4d5d8b52a540a508e1115f77cd7fa9a291243f501223", 0, "HASH160 0x14 0xb1ce99298d5f07364b57b1e5c9cc00be0b04a954 EQUAL"]],
      |"01000000012312503f2491a2a97fcd775f11e108a540a5528b5d4dee7a3c68ae4add01dab300000000fdfe000048304502207aacee820e08b0b174e248abd8d7a34ed63b5da3abedb99934df9fddd65c05c4022100dfe87896ab5ee3df476c2655f9fbe5bd089dccbef3e4ea05b5d121169fe7f5f401483045022100f6649b0eddfdfd4ad55426663385090d51ee86c3481bdc6b0c18ea6c0ece2c0b0220561c315b07cffa6f7dd9df96dbae9200c2dee09bf93cc35ca05e6cdf613340aa014c695221031d11db38972b712a9fe1fc023577c7ae3ddb4a3004187d41c45121eecfdbb5b7210207ec36911b6ad2382860d32989c7b8728e9489d7bbc94a6b5509ef0029be128821024ea9fac06f666a4adc3fc1357b7bec1fd0bdece2b9d08579226a8ebde53058e453aeffffffff0180380100000000001976a914c9b99cddf847d10685a4fabaa0baf505f7c3dfab88ac00000000", "P2SH"],
      |
      |
      |["The following is a tweaked form of 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63"],
      |["It is an OP_CHECKMULTISIG with the dummy value missing"],
      |[[["60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1", 0, "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"]],
      |"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004847304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000", "P2SH"],
      |
      |
      |["CHECKMULTISIG SCRIPT_VERIFY_NULLDUMMY tests:"],
      |
      |["The following is a tweaked form of 23b397edccd3740a74adb603c9756370fafcde9bcc4483eb271ecad09a94dd63"],
      |["It is an OP_CHECKMULTISIG with the dummy value set to something other than an empty string"],
      |[[["60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1", 0, "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"]],
      |"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004a010047304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000", "P2SH,NULLDUMMY"],
      |
      |["As above, but using a OP_1"],
      |[[["60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1", 0, "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"]],
      |"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000495147304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000", "P2SH,NULLDUMMY"],
      |
      |["As above, but using a OP_1NEGATE"],
      |[[["60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1", 0, "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"]],
      |"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba26000000000494f47304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000", "P2SH,NULLDUMMY"],
      |
      |["As above, but with the dummy byte missing"],
      |[[["60a20bd93aa49ab4b28d514ec10b06e1829ce6818ec06cd3aabd013ebcdc4bb1", 0, "1 0x41 0x04cc71eb30d653c0c3163990c47b976f3fb3f37cccdcbedb169a1dfef58bbfbfaff7d8a473e7e2e6d317b87bafe8bde97e3cf8f065dec022b51d11fcdd0d348ac4 0x41 0x0461cbdcc5409fb4b4d42b51d33381354d80e550078cb532a34bfa2fcfdeb7d76519aecc62770f5b0e4ef8551946d8a540911abe3e7854a26f39f58b25c15342af 2 OP_CHECKMULTISIG"]],
      |"0100000001b14bdcbc3e01bdaad36cc08e81e69c82e1060bc14e518db2b49aa43ad90ba260000000004847304402203f16c6f40162ab686621ef3000b04e75418a0c0cb2d8aebeac894ae360ac1e780220ddc15ecdfc3507ac48e1681a33eb60996631bf6bf5bc0a0682c4db743ce7ca2b01ffffffff0140420f00000000001976a914660d4ef3a743e3e696ad990364e555c271ad504b88ac00000000", "P2SH,NULLDUMMY"],
      |
      |
      |["Empty stack when we try to run CHECKSIG"],
      |[[["ad503f72c18df5801ee64d76090afe4c607fb2b822e9b7b63c5826c50e22fc3b", 0, "0x21 0x027c3a97665bf283a102a587a62a30a0c102d4d3b141015e2cae6f64e2543113e5 CHECKSIG NOT"]],
      |"01000000013bfc220ec526583cb6b7e922b8b27f604cfe0a09764de61e80f58dc1723f50ad0000000000ffffffff0101000000000000002321027c3a97665bf283a102a587a62a30a0c102d4d3b141015e2cae6f64e2543113e5ac00000000", "P2SH"],
      |
      |
      |["Inverted versions of tx_valid CODESEPARATOR IF block tests"],
      |
      |["CODESEPARATOR in an unexecuted IF block does not change what is hashed"],
      |[[["a955032f4d6b0c9bfe8cad8f00a8933790b9c1dc28c82e0f48e75b35da0e4944", 0, "IF CODESEPARATOR ENDIF 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 CHECKSIGVERIFY CODESEPARATOR 1"]],
      |"010000000144490eda355be7480f2ec828dcc1b9903793a8008fad8cfe9b0c6b4d2f0355a9000000004a48304502207a6974a77c591fa13dff60cabbb85a0de9e025c09c65a4b2285e47ce8e22f761022100f0efaac9ff8ac36b10721e0aae1fb975c90500b50c56e8a0cc52b0403f0425dd0151ffffffff010000000000000000016a00000000", "P2SH"],
      |
      |["As above, with the IF block executed"],
      |[[["a955032f4d6b0c9bfe8cad8f00a8933790b9c1dc28c82e0f48e75b35da0e4944", 0, "IF CODESEPARATOR ENDIF 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 CHECKSIGVERIFY CODESEPARATOR 1"]],
      |"010000000144490eda355be7480f2ec828dcc1b9903793a8008fad8cfe9b0c6b4d2f0355a9000000004a483045022100fa4a74ba9fd59c59f46c3960cf90cbe0d2b743c471d24a3d5d6db6002af5eebb02204d70ec490fd0f7055a7c45f86514336e3a7f03503dacecabb247fc23f15c83510100ffffffff010000000000000000016a00000000", "P2SH"],
      |
      |["CHECKLOCKTIMEVERIFY tests"],
      |
      |["By-height locks, with argument just beyond tx nLockTime"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1 CHECKLOCKTIMEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "499999999 CHECKLOCKTIMEVERIFY 1"]],
      |"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000fe64cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["By-time locks, with argument just beyond tx nLockTime (but within numerical boundaries)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "500000001 CHECKLOCKTIMEVERIFY 1"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000065cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4294967295 CHECKLOCKTIMEVERIFY 1"]],
      |"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000feffffff", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Argument missing"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "CHECKLOCKTIMEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000001b1010000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Argument negative with by-blockheight nLockTime=0"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "-1 CHECKLOCKTIMEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Argument negative with by-blocktime nLockTime=500,000,000"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "-1 CHECKLOCKTIMEVERIFY 1"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000065cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000004005194b1010000000100000000000000000002000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Input locked"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0 CHECKLOCKTIMEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000251b1ffffffff0100000000000000000002000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Another input being unlocked isn't sufficient; the CHECKLOCKTIMEVERIFY-using input must be unlocked"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0 CHECKLOCKTIMEVERIFY 1"] ,
      |  ["0000000000000000000000000000000000000000000000000000000000000200", 1, "1"]],
      |"010000000200010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00020000000000000000000000000000000000000000000000000000000000000100000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Argument/tx height/time mismatch, both versions"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0 CHECKLOCKTIMEVERIFY 1"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000065cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000251b100000000010000000000000000000065cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "499999999 CHECKLOCKTIMEVERIFY 1"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000065cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "500000000 CHECKLOCKTIMEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "500000000 CHECKLOCKTIMEVERIFY 1"]],
      |"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ff64cd1d", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Argument 2^32 with nLockTime=2^32-1"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4294967296 CHECKLOCKTIMEVERIFY 1"]],
      |"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ffffffff", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Same, but with nLockTime=2^31-1"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "2147483648 CHECKLOCKTIMEVERIFY 1"]],
      |"0100000001000100000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000ffffff7f", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["6 byte non-minimally-encoded arguments are invalid even if their contents are valid"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x06 0x000000000000 CHECKLOCKTIMEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Failure due to failing CHECKLOCKTIMEVERIFY in scriptSig"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1"]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000251b1000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["Failure due to failing CHECKLOCKTIMEVERIFY in redeemScript"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0xc5b93064159b3b2d6ab506a41b1f50463771b988 EQUAL"]],
      |"0100000001000100000000000000000000000000000000000000000000000000000000000000000000030251b1000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"],
      |
      |["A transaction with a non-standard DER signature."],
      |[[["b1dbc81696c8a9c0fccd0693ab66d7c368dbc38c0def4e800685560ddd1b2132", 0, "DUP HASH160 0x14 0x4b3bd7eba3bc0284fd3007be7f3be275e94f5826 EQUALVERIFY CHECKSIG"]],
      |"010000000132211bdd0d568506804eef0d8cc3db68c3d766ab9306cdfcc0a9c89616c8dbb1000000006c493045022100c7bb0faea0522e74ff220c20c022d2cb6033f8d167fb89e75a50e237a35fd6d202203064713491b1f8ad5f79e623d0219ad32510bfaa1009ab30cbee77b59317d6e30001210237af13eb2d84e4545af287b919c2282019c9691cc509e78e196a9d8274ed1be0ffffffff0100000000000000001976a914f1b3ed2eda9a2ebe5a9374f692877cdf87c0f95b88ac00000000", "P2SH,DERSIG"],
      |
      |["CHECKSEQUENCEVERIFY tests"],
      |
      |["By-height locks, with argument just beyond txin.nSequence"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4259839 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000feff40000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["By-time locks, with argument just beyond txin.nSequence (but within numerical boundaries)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4194305 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4259839 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000feff40000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Argument missing"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Argument negative with by-blockheight txin.nSequence=0"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "-1 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Argument negative with by-blocktime txin.nSequence=CTxIn::SEQUENCE_LOCKTIME_TYPE_FLAG"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "-1 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Argument/tx height/time mismatch, both versions"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "65535 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4194304 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4259839 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["6 byte non-minimally-encoded arguments are invalid even if their contents are valid"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x06 0x000000000000 CHECKSEQUENCEVERIFY 1"]],
      |"020000000100010000000000000000000000000000000000000000000000000000000000000000000000ffff00000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Failure due to failing CHECKSEQUENCEVERIFY in scriptSig"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "1"]],
      |"02000000010001000000000000000000000000000000000000000000000000000000000000000000000251b2000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Failure due to failing CHECKSEQUENCEVERIFY in redeemScript"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "HASH160 0x14 0x7c17aff532f22beb54069942f9bf567a66133eaf EQUAL"]],
      |"0200000001000100000000000000000000000000000000000000000000000000000000000000000000030251b2000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Failure due to insufficient tx.nVersion (<2)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0 CHECKSEQUENCEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "4194304 CHECKSEQUENCEVERIFY 1"]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000040000100000000000000000000000000", "P2SH,CHECKSEQUENCEVERIFY"],
      |
      |["Unknown witness program version (with DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x51", 1000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x60 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 2000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 2, "0x51", 3000]],
      |"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151b80b00000000000001510002483045022100a3cec69b52cba2d2de623ffffffffff1606184ea55476c0f8189fda231bc9cbb022003181ad597f7c380a7d1c740286b1d022b8b04ded028b833282e055e03b8efef812103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000", "P2SH,WITNESS,DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM"],
      |
      |["Unknown length for witness program v0"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x51", 1000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x00 0x15 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3fff", 2000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 2, "0x51", 3000]],
      |"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff04b60300000000000001519e070000000000000151860b0000000000000100960000000000000001510002473044022022fceb54f62f8feea77faac7083c3b56c4676a78f93745adc8a35800bc36adfa022026927df9abcf0a8777829bcfcce3ff0a385fa54c3f9df577405e3ef24ee56479022103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000", "P2SH,WITNESS"],
      |
      |["Witness with SigHash Single|AnyoneCanPay (same index output value changed)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x51", 1000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 2000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 2, "0x51", 3000]],
      |"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e80300000000000001516c070000000000000151b80b0000000000000151000248304502210092f4777a0f17bf5aeb8ae768dec5f2c14feabf9d1fe2c89c78dfed0f13fdb86902206da90a86042e252bcd1e80a168c719e4a1ddcc3cebea24b9812c5453c79107e9832103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000", "P2SH,WITNESS"],
      |
      |["Witness with SigHash None|AnyoneCanPay (input sequence changed)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x51", 1000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 2000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 2, "0x51", 3000]],
      |"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff000100000000000000000000000000000000000000000000000000000000000001000000000100000000010000000000000000000000000000000000000000000000000000000000000200000000ffffffff00000248304502210091b32274295c2a3fa02f5bce92fb2789e3fc6ea947fbe1a76e52ea3f4ef2381a022079ad72aefa3837a2e0c033a8652a59731da05fa4a813f4fc48e87c075037256b822103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000", "P2SH,WITNESS"],
      |
      |["Witness with SigHash All|AnyoneCanPay (third output value changed)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x51", 1000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 2000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 2, "0x51", 3000]],
      |"0100000000010300010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff00010000000000000000000000000000000000000000000000000000000000000200000000ffffffff03e8030000000000000151d0070000000000000151540b00000000000001510002483045022100a3cec69b52cba2d2de623eeef89e0ba1606184ea55476c0f8189fda231bc9cbb022003181ad597f7c380a7d1c740286b1d022b8b04ded028b833282e055e03b8efef812103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000", "P2SH,WITNESS"],
      |
      |["Witness with a push of 521 bytes"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x00 0x20 0x33198a9bfef674ebddb9ffaa52928017b8472791e54c609cb95f278ac6b1e349", 1000]],
      |"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff010000000000000000015102fd0902000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002755100000000", "P2SH,WITNESS"],
      |
      |["Witness with unknown version which push false on the stack should be invalid (even without DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x60 0x02 0x0000", 2000]],
      |"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff010000000000000000015101010100000000", "P2SH,WITNESS"],
      |
      |["Witness program should leave clean stack"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x00 0x20 0x2f04a3aa051f1f60d695f6c44c0c3d383973dfd446ace8962664a76bb10e31a8", 2000]],
      |"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01000000000000000001510102515100000000", "P2SH,WITNESS"],
      |
      |["Witness v0 with a push of 2 bytes"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x00 0x02 0x0001", 2000]],
      |"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff010000000000000000015101040002000100000000", "P2SH,WITNESS"],
      |
      |["Unknown witness version with non empty scriptSig"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x60 0x02 0x0001", 2000]],
      |"01000000010001000000000000000000000000000000000000000000000000000000000000000000000151ffffffff010000000000000000015100000000", "P2SH,WITNESS"],
      |
      |["Non witness Single|AnyoneCanPay hash input's position (permutation)"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x21 0x03596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71 CHECKSIG", 1000],
      |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x21 0x03596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71 CHECKSIG", 1001]],
      |"010000000200010000000000000000000000000000000000000000000000000000000000000100000049483045022100acb96cfdbda6dc94b489fd06f2d720983b5f350e31ba906cdbd800773e80b21c02200d74ea5bdf114212b4bbe9ed82c36d2e369e302dff57cb60d01c428f0bd3daab83ffffffff0001000000000000000000000000000000000000000000000000000000000000000000004847304402202a0b4b1294d70540235ae033d78e64b4897ec859c7b6f1b2b1d8a02e1d46006702201445e756d2254b0f1dfda9ab8e1e1bc26df9668077403204f32d16a49a36eb6983ffffffff02e9030000000000000151e803000000000000015100000000", "P2SH,WITNESS"],
      |
      |["P2WSH with a redeem representing a witness scriptPubKey should fail"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x00 0x20 0x34b6c399093e06cf9f0f7f660a1abcfe78fcf7b576f43993208edd9518a0ae9b", 1000]],
      |"0100000000010100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff0001045102010100000000", "P2SH,WITNESS"],
      |
      |["33 bytes push should be considered a witness scriptPubKey"],
      |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x60 0x21 0xff25429251b5a84f452230a3c75fd886b7fc5a7865ce4a7bb7a9d7c5be6da3dbff", 1000]],
      |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000ffffffff01e803000000000000015100000000", "P2SH,WITNESS,DISCOURAGE_UPGRADABLE_WITNESS_PROGRAM"],
      |
      |["FindAndDelete tests"],
      |["This is a test of FindAndDelete. The first tx is a spend of normal scriptPubKey and the second tx is a spend of bare P2WSH."],
      |["The redeemScript/witnessScript is CHECKSIGVERIFY <0x30450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01>."],
      |["The signature is <0x30450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01> <pubkey>,"],
      |["where the pubkey is obtained through key recovery with sig and the wrong sighash."],
      |["This is to show that FindAndDelete is applied only to non-segwit scripts"],
      |["To show that the tests are 'correctly wrong', they should pass by modifying OP_CHECKSIG under interpreter.cpp"],
      |["by replacing (sigversion == SIGVERSION_BASE) with (sigversion != SIGVERSION_BASE)"],
      |["Non-segwit: wrong sighash (without FindAndDelete) = 1ba1fe3bc90c5d1265460e684ce6774e324f0fabdf67619eda729e64e8b6bc08"],
      |[[["f18783ace138abac5d3a7a5cf08e88fe6912f267ef936452e0c27d090621c169", 7000, "HASH160 0x14 0x0c746489e2d83cdbb5b90b432773342ba809c134 EQUAL", 200000]],
      |"010000000169c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f1581b0000b64830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e012103b12a1ec8428fc74166926318c15e17408fea82dbb157575e16a8c365f546248f4aad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e01ffffffff0101000000000000000000000000", "P2SH,WITNESS"],
      |["BIP143: wrong sighash (with FindAndDelete) = 71c9cd9b2869b9c70b01b1f0360c148f42dee72297db312638df136f43311f23"],
      |[[["f18783ace138abac5d3a7a5cf08e88fe6912f267ef936452e0c27d090621c169", 7500, "0x00 0x20 0x9e1be07558ea5cc8e02ed1d80c0911048afad949affa36d5c3951e3159dbea19", 200000]],
      |"0100000000010169c12106097dc2e0526493ef67f21269fe888ef05c7a3a5dacab38e1ac8387f14c1d000000ffffffff01010000000000000000034830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e012102a9d7ed6e161f0e255c10bbfcca0128a9e2035c2c8da58899c54d22d3a31afdef4aad4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0100000000", "P2SH,WITNESS"],
      |["This is multisig version of the FindAndDelete tests"],
      |["Script is 2 CHECKMULTISIGVERIFY <sig1> <sig2> DROP"],
      |["52af4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c0395960175"],
      |["Signature is 0 <sig1> <sig2> 2 <key1> <key2>"],
      |["Should pass by replacing (sigversion == SIGVERSION_BASE) with (sigversion != SIGVERSION_BASE) under OP_CHECKMULTISIG"],
      |["Non-segwit: wrong sighash (without FindAndDelete) = 4bc6a53e8e16ef508c19e38bba08831daba85228b0211f323d4cb0999cf2a5e8"],
      |[[["9628667ad48219a169b41b020800162287d2c0f713c04157e95c484a8dcb7592", 7000, "HASH160 0x14 0x5748407f5ca5cdca53ba30b79040260770c9ee1b EQUAL", 200000]],
      |"01000000019275cb8d4a485ce95741c013f7c0d28722160008021bb469a11982d47a662896581b0000fd6f01004830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c039596015221023fd5dd42b44769c5653cbc5947ff30ab8871f240ad0c0e7432aefe84b5b4ff3421039d52178dbde360b83f19cf348deb04fa8360e1bf5634577be8e50fafc2b0e4ef4c9552af4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c0395960175ffffffff0101000000000000000000000000", "P2SH,WITNESS"],
      |["BIP143: wrong sighash (with FindAndDelete) = 17c50ec2181ecdfdc85ca081174b248199ba81fff730794d4f69b8ec031f2dce"],
      |[[["9628667ad48219a169b41b020800162287d2c0f713c04157e95c484a8dcb7592", 7500, "0x00 0x20 0x9b66c15b4e0b4eb49fa877982cafded24859fe5b0e2dbfbe4f0df1de7743fd52", 200000]],
      |"010000000001019275cb8d4a485ce95741c013f7c0d28722160008021bb469a11982d47a6628964c1d000000ffffffff0101000000000000000007004830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c03959601010221023cb6055f4b57a1580c5a753e19610cafaedf7e0ff377731c77837fd666eae1712102c1b1db303ac232ffa8e5e7cc2cf5f96c6e40d3e6914061204c0541cb2043a0969552af4830450220487fb382c4974de3f7d834c1b617fe15860828c7f96454490edd6d891556dcc9022100baf95feb48f845d5bfc9882eb6aeefa1bc3790e39f59eaa46ff7f15ae626c53e0148304502205286f726690b2e9b0207f0345711e63fa7012045b9eb0f19c2458ce1db90cf43022100e89f17f86abc5b149eba4115d4f128bcf45d77fb3ecdd34f594091340c039596017500000000", "P2SH,WITNESS"],
      |
      |["Make diffs cleaner by leaving a comment here without comma at the end"]
      |]
      |""".stripMargin

}
