package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.ZCashRegTest
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.protocol.Address
import org.bitcoins.core.protocol.script.{ MultiSignatureScriptPubKey, P2SHScriptPubKey }
import org.bitcoins.core.protocol.transaction.{ Transaction, TransactionOutPoint, TransactionOutput }
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.ZcashUTXOSpendingInfo
import org.scalatest.{ FlatSpec, MustMatchers }

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

class ZCashTxBuilderTest extends FlatSpec with MustMatchers {
  val timeout = 5.seconds
  "ZCashBuilder" must "be able to validly spend a p2pkh output on the zcash network" in {
    /*    cstewart-m01:docker-zcash-regtest cstewart$ docker start zcash-regtest
    zcash-regtest
    cstewart-m01:docker-zcash-regtest cstewart$ docker exec zcash-regtest zcash-cli generate 101 2>&1 >/dev/null
    cstewart-m01:docker-zcash-regtest cstewart$ docker exec zcash-regtest zcash-cli getnewaddress
    tmDLXeHwssXz1fchhPHvdQYE6VnbhEVc1ZZ
    cstewart-m01:docker-zcash-regtest cstewart$ docker exec zcash-regtest zcash-cli sendtoaddress tmDLXeHwssXz1fchhPHvdQYE6VnbhEVc1ZZ 1
    092c1284eaed8519195e948729b983b7a3943e2f941be13453cafca53e377aff
      cstewart-m01:docker-zcash-regtest cstewart$ docker exec zcash-regtest zcash-cli getrawtransaction 092c1284eaed8519195e948729b983b7a3943e2f941be13453cafca53e377aff
      0100000001a6b721a495b9f13d36a900934e7573d5b934159fc5d8d61ffa9e4a22a0f16880000000006a47304402203af14fe4134c1674f8e76b8e12e343677bab2f1c9b8a4a39a47bf437c2f28df302206ac89fcd330129b4107e39750619e397f75a69680394770a3a0b36666311e8a6012103aea2894d00e17314d6f7b0da3a17011f7e6887111e68b0e312796f605e14e8b2feffffff0200e1f505000000001976a91427bff5da7e336fa2b1309f81cf9b74b3665472e688ac1ee8a435000000001976a914e08d344ff16f74402354933e36fddf5eb206b64c88ac5b000000
      cstewart-m01:docker-zcash-regtest cstewart$ docker exec zcash-regtest zcash-cli dumpprivkey tmDLXeHwssXz1fchhPHvdQYE6VnbhEVc1ZZ
      cTyw5hyEdAVRLX4BFdh53afYEqGLvw8y3gvVBHWp9xzZqCzBx3xB
      cstewart-m01:docker-zcash-regtest cstewart$ docker exec zcash-regtest zcash-cli getnewaddress
tmA3mCo2QLqeX81tba3WC8eqdMj8phk8Hus
      tmNvSvUdNhRujft8eq6AJvi7dAUS3NnoGFo

      //this sendrawtransaction call is sending the output of the println below:
      //$ docker exec zcash-regtest zcash-cli sendrawtransaction 0100000001ff7a373ea5fcca5334e11b942f3e94a3b783b92987945e191985edea84122c09000000006b483045022100d54fe46df33ebbdba82639af28a789e6b6d80fd97c5bb6ab1668d65268c4a9ba022073a192fe77b49bdefa5613e3ce5a4894cf635a370a9d1ecb0e0698c23cd9a1d7012103aea2894d00e17314d6f7b0da3a17011f7e6887111e68b0e312796f605e14e8b2ffffffff02404b4c00000000001976a91403abd4cf548f82895151dd45bf1999be973edb0988ac5b91a905000000001976a91490e34b5489bace82925b80340b640429bf59694588ac00000000
      // 57f53cbdc4e0d2a63cb3c558c39a744e37cd9aa6bb1516b5df720a6cedabab3d
      //$ docker exec zcash-regtest zcash-cli generate 6
      //$ docker exec zcash-regtest zcash-cli getrawtransaction 57f53cbdc4e0d2a63cb3c558c39a744e37cd9aa6bb1516b5df720a6cedabab3d 1
      //  "blockhash": "0b654ca591b90b51c6eb14706b2f5dff52ae93ed4944613a10ba74d48acb6c94",
  "confirmations": 6,
  "time": 1522088897,
  "blocktime": 1522088897
      //yay! 6 confirmations. Means it was validly signed by ZCashTxBuilder. Only valid for p2pkh spks for now
}

      */

    val creditingTxHex = "0100000001a6b721a495b9f13d36a900934e7573d5b934159fc5d8d61ffa9e4a22a0f16880000000006a47304402203af14fe4134c1674f8e76b8e12e343677bab2f1c9b8a4a39a47bf437c2f28df302206ac89fcd330129b4107e39750619e397f75a69680394770a3a0b36666311e8a6012103aea2894d00e17314d6f7b0da3a17011f7e6887111e68b0e312796f605e14e8b2feffffff0200e1f505000000001976a91427bff5da7e336fa2b1309f81cf9b74b3665472e688ac1ee8a435000000001976a914e08d344ff16f74402354933e36fddf5eb206b64c88ac5b000000"
    val creditingTx = Transaction(creditingTxHex)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val output = creditingTx.outputs(outPoint.vout.toInt)
    val privKey = ECPrivateKey.fromWIFToPrivateKey("cTyw5hyEdAVRLX4BFdh53afYEqGLvw8y3gvVBHWp9xzZqCzBx3xB")
    val destinationAddr = Address("tmA3mCo2QLqeX81tba3WC8eqdMj8phk8Hus").get
    val destinationSPK = destinationAddr.scriptPubKey
    val destinationOutput = TransactionOutput(Satoshis(Int64(5000000)), destinationSPK)
    val changeAddr = Address("tmNvSvUdNhRujft8eq6AJvi7dAUS3NnoGFo")
    val changeSPK = changeAddr.get.scriptPubKey
    val feeRate = SatoshisPerVirtualByte(Satoshis(Int64(5)))
    val network = ZCashRegTest
    val utxos = ZcashUTXOSpendingInfo(outPoint, output, Seq(privKey), None, None, HashType.sigHashAll)
    val txBuilder = ZcashTxBuilder(Seq(destinationOutput), Seq(utxos), feeRate, changeSPK, network)
    val signedTx = txBuilder.flatMap(_.sign)
    println(Await.result(signedTx, timeout).hex)

  }

  it must "be able to validly spend a p2sh multisig output on the zcash network" in {
    /*
      $ docker exec zcash-regtest zcash-cli getnewaddress
      tmGXDxSP1hzKdWDugkGtmGpKGCKudKLTW5g
      $ docker exec zcash-regtest zcash-cli getnewaddress
      tmBmsLNWLRvfbjhtzFnijSHnvAMTf1yVPoo
      $ docker exec zcash-regtest zcash-cli getnewaddress
      tmWjUwYSWEm2xHFoFEXqfSLtWKwVMjbaVrJ
      $ docker exec zcash-regtest zcash-cli dumpprivkey tmGXDxSP1hzKdWDugkGtmGpKGCKudKLTW5g
      cTurnj6bXgpuGqw9T3TKvDo839PqxXV3GYtwmBUeXG3x98cv8CvB
      $ docker exec zcash-regtest zcash-cli dumpprivkey tmBmsLNWLRvfbjhtzFnijSHnvAMTf1yVPoo
      cUS9eReHbKJKsMwu3uLqByrvyggWJZNp9tK1w3fWoYD5KjojdAHD
      $ docker exec zcash-regtest zcash-cli dumpprivkey tmWjUwYSWEm2xHFoFEXqfSLtWKwVMjbaVrJ
      cVz2nr2PXxXS6VpW2qvPE6tYjV4CfcSYhW6A3ezPJZfTV2wyJNCS
      $ docker exec zcash-regtest zcash-cli createmultisig 2 '''
        [
        "tmWjUwYSWEm2xHFoFEXqfSLtWKwVMjbaVrJ",
        "tmBmsLNWLRvfbjhtzFnijSHnvAMTf1yVPoo",
        "tmGXDxSP1hzKdWDugkGtmGpKGCKudKLTW5g"]'''
        {
          "address": "t2QsZbrSWMmoEmBaHXcADAKAiMKap4cWZ82",
          "redeemScript": "522103bb4385fff73754fe1d5be8799ef488677a170ec3698925970ee7c15ba950f7f821039764988dfa780ffd0506b806e7de4e3c1ebe1a281956be0c032c28b7a05dea2621020a0c850cbafc5011b10b54435b278090d32e6e15a9b5c454c662a1fc71ab6e4953ae"
        }
       $ docker exec zcash-regtest zcash-cli sendtoaddress t2QsZbrSWMmoEmBaHXcADAKAiMKap4cWZ82 1
         4cf3851351f4bdf6c1e512767be756e194acdebc47251d96342c04ba93367258
       $ docker exec zcash-regtest zcash-cli getrawtransaction 4cf3851351f4bdf6c1e512767be756e194acdebc47251d96342c04ba93367258
         0100000001ff7a373ea5fcca5334e11b942f3e94a3b783b92987945e191985edea84122c09010000006b483045022100bf3af715d1cfc1f6526d9e4a1785557db24eb1e4a80eae76572794279c73aa1802206659f2d52cf595cd8ac7d7ddf8c9163df8f2fdb02fd5687cd336356bd5c15352012102b808848a1d4409e47c75df14e181125f71ebfaf5e164105888a907102f7d2326feffffff023e06af2f000000001976a914399aa8aca767fca7b45eff6dbfd88f23c4f257d688ac00e1f5050000000017a914c8fe2b205f802d2ca103b27ead38d0d199f321d48761000000
       $ docker exec zcash-regtest zcash-cli getnewaddress
         tmVB1PX3HGyKv5y2UbT6D8cWhDH6Z4BcqoR
      $ docker exec zcash-regtest zcash-cli getnewaddress
        tmHsHDDtWvnRV2eUZEEcyQ6EYKE6HJqEXkn
       $ docker exec zcash-regtest zcash-cli sendrawtransaction 010000000158723693ba042c34961d2547bcdeac94e156e77b7612e5c1f6bdf4511385f34c01000000fdfd00004730440220711b5a5b9b0545f306ecbf7c28ab9efd1e96326a6c9054cfee7ac06e8340242702206796342a681489aede32f4255c252f01d4036061cc5e30fbe33678a2a8bc83a1014830450221008bb23705f4b73ebd6796f3cfb971d202a1550b5f582e9f9ddd12b35e7df6e66f02202c1330538b2ca023bad23599cb18a6f96148cbeef60f1527ddef3b6ba9a6d996014c69522103bb4385fff73754fe1d5be8799ef488677a170ec3698925970ee7c15ba950f7f821039764988dfa780ffd0506b806e7de4e3c1ebe1a281956be0c032c28b7a05dea2621020a0c850cbafc5011b10b54435b278090d32e6e15a9b5c454c662a1fc71ab6e4953aeffffffff02404b4c00000000001976a914d574f94fce3a53bf89b7b4080220ef20456acdb788ac818ea905000000001976a91459714f965909e0edd68110116e6ed65102a03bbe88ac00000000
314b8b35b8b87b246024605f7eac1fc79a4f7bb007dcd67e9f526a0143516374
      $ docker exec zcash-regtest zcash-cli generate 6
      $ docker exec zcash-regtest zcash-cli getrawtransaction 314b8b35b8b87b246024605f7eac1fc79a4f7bb007dcd67e9f526a0143516374
        "blockhash": "03db5b75352b936a8927c0315f06bbe1fbc07ba7c4854b92e36773ad31523e8a",
        "confirmations": 6,
        "time": 1522165518,
        "blocktime": 1522165518
       }


      */

    val privKey1 = ECPrivateKey.fromWIFToPrivateKey("cVz2nr2PXxXS6VpW2qvPE6tYjV4CfcSYhW6A3ezPJZfTV2wyJNCS")
    val privKey2 = ECPrivateKey.fromWIFToPrivateKey("cUS9eReHbKJKsMwu3uLqByrvyggWJZNp9tK1w3fWoYD5KjojdAHD")
    val privKey3 = ECPrivateKey.fromWIFToPrivateKey("cTurnj6bXgpuGqw9T3TKvDo839PqxXV3GYtwmBUeXG3x98cv8CvB")
    val privKeys = Seq(privKey1, privKey2, privKey3)
    val pubKeys = privKeys.map(_.publicKey)
    val multisig = MultiSignatureScriptPubKey(2, pubKeys)
    val p2sh = P2SHScriptPubKey(multisig)
    val creditingTxHex = "0100000001ff7a373ea5fcca5334e11b942f3e94a3b783b92987945e191985edea84122c09010000006b483045022100bf3af715d1cfc1f6526d9e4a1785557db24eb1e4a80eae76572794279c73aa1802206659f2d52cf595cd8ac7d7ddf8c9163df8f2fdb02fd5687cd336356bd5c15352012102b808848a1d4409e47c75df14e181125f71ebfaf5e164105888a907102f7d2326feffffff023e06af2f000000001976a914399aa8aca767fca7b45eff6dbfd88f23c4f257d688ac00e1f5050000000017a914c8fe2b205f802d2ca103b27ead38d0d199f321d48761000000"
    val creditingTx = Transaction(creditingTxHex)
    val (output, idx) = creditingTx.outputs.zipWithIndex.find {
      case (o, idx) =>
        o.scriptPubKey == p2sh
    }.get
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32(idx))
    val destinationSPK = Address("tmVB1PX3HGyKv5y2UbT6D8cWhDH6Z4BcqoR").get.scriptPubKey
    val destinationOutput = TransactionOutput(Satoshis(Int64(5000000)), destinationSPK)
    val changeSPK = Address("tmHsHDDtWvnRV2eUZEEcyQ6EYKE6HJqEXkn").get.scriptPubKey
    val feeRate = SatoshisPerVirtualByte(Satoshis(Int64(5)))
    val network = ZCashRegTest
    val utxos = ZcashUTXOSpendingInfo(outPoint, output, privKeys, Some(multisig), None, HashType.sigHashAll)
    val txBuilder = ZcashTxBuilder(Seq(destinationOutput), Seq(utxos), feeRate, changeSPK, network)
    val signedTx = txBuilder.flatMap(_.sign)
    println(Await.result(signedTx, timeout).hex)
  }
}

