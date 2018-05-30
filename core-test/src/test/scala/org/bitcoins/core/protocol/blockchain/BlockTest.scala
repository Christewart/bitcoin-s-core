package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{ Int32, Int64, UInt32, UInt64 }
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.serializers.RawSerializerHelper
import org.bitcoins.core.serializers.script.RawScriptSignatureParser
import org.bitcoins.core.util.{ BitcoinSLogger, BitcoinSUtil }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 7/15/16.
 */
class BlockTest extends FlatSpec with MustMatchers {

  private val logger = BitcoinSLogger.logger

  /*  "Block" must "deserialize and serialize a block with two txs" in {
    val hex = "b0aa0dced271237b5badb4365c33bc42b8fdb05c3defa03e75dcae425f42ddd210d69b8695e0f3cdf1eb9f56d0daa3c1e384204faa20792217d3103217773f4177b6b0f067079ac1ee733798f3bef9a0" +
      "02eb9a240f06a4c916eb2e865ee48befdea6840273bbceb3546c0618d03b420c16fa10e80571bc81fb77fd9f05004730450220769b7dc2d74accb2f3f0c0d360260905129d72ed097ff5fe53072191c7303bc7022100d01c6d97c9fd9887d898349b3f12091f1233d9db5e9facbcf8599efb9db259574730450220751d9e59967c00b706f19642f93672c70d47141ab2516e33c8c20f206e6838bb022100bde2f1d87ec1b2a2289127ba2841cdc3c6dcf65c3c2fe3f74b53535dd8170f46483046022100d3e34631872344a41b4a1b1d2c63b484aeaf6edc6fc5d836fbffbcbbd5e9121002210094e11649a9da76020ad4188f7a5f588f01cab0cb8d6591c122ae0db03fa03a6d473045022100bc41ad4c6af0d7e3d2a61a3d7fdfe7116927bd563b1112a3582261e8a9aa9b44022041c58ec9163d88c158475630493c62079c1740727a44c6ca862b07f7d273022e46304402207aa8d6e05a81e2f17840e55f6ca96ded6abf13ed970c5ef6a5a7f1af1d9a552002201b4898853632e1c12ea2de3b93e257e1d06fee9f01990375383da5aa88c0eb7946304402205f055b0f557bab39f4ce539075691667e20cd5ac64752320ea5a12caa03db2b6022053cfb6887e0068568388f986023f0e08b214a7e598d7c71192ad81dbd6b1218b473045022004d31129c86aa93ea0bd4e43e1daa1101ccfc065e5e5a98dcaa5de2cf663f2bf022100bc3208e7afd0682c544c692c9f7616d82feb686808b05b0c1d3f98cc38bdac0e473045022100f0385c6ca49e5198befa6663c30af80f873449d553f9fbea188167b181d0b15d022059641c26eeb3c7e0af7fd2e2edd9b1a3db2946497ac21ca71cfbcfec94d69fc9473045022000dbb499b48c8c2026128b78af3e5b9b6e4ff8e505a8b6effa2464902d2635a2022100b205e57aa35a944d7d8d001a3f69640582f7dc9e6703fa69ad37c210018dcc8c473045022100cb31ef46254466086aca276d8d46a3ffa8035a042751242804df36d0151adacd02201743ac1e0f9ee3a85129e56a9860ab612bf2c1e0afb80d4ddbd49521401f579c4830460221009f880f98cb0566027fb10e38feb7a63c875128e129ea5565fba2ad8db25ba517022100ff49ed12c73bb4d90b57de30c623906e92ccbdd62dad21c88fa5d7b16dae0854473045022100ab3d3db5204cbf011c2f9fffd687ee1fd026ddd3c32e22d91a37b328e1cff68402202b93cd46eb3473aec0e422b89644b6a65803f23c3d1de942b03e8e5fef02c6fb483046022100a62f8e18cf530cf2798a6a3ece89b22a0ad68f787c0b4ea5fbaa1f1dc7c6af7102210085de71bf5c6fead1cebf903b0e63bd4b0502b4784ebc3c56122bd56a8e5a949f47304502201ef3f95b628b1d11ede235b91699f205c20583d974131044b1c28c862b767fa9022100f79ac57c6824f232da1b38135a07f62b8e93b571bcce0e68faa51ebeccd5b76d473045022100dd5fffb857af413ac80d14f791a9afbf0b966130630e8bf385c2da551c41617502207173e35682194bf079f746a4cf66fd58b9435f0fb59d2ddb5a9a481589e2b6d647304502205db8ddf57d042f4fc6496d0e9816695ea343894b9fbc23bba204e276b77ea751022100fee4589a862f469f8d8bce588fe2f8a22d787dab01fa64a97abc25597e340b7a46304402202a85eefb6d150d463c55aae8d0686f654f8086f0a310b52b7e8cb00f26af2ca102201987cb08cbeca18d893ce4eb4c0fcfe7a6a1905bfe0b60da035a37cb4f8423ef46304402201b5c4cfc6c3d97bbd63a2ddcab88861d697e6e069f474da00d0b95f41f6b4caa0220545a2645531cdaf51e3bd8bc5a526a5d821b1d8d01e72f1dc49642bb438fd3e6463044022051b677c9cb95804e8879f9ece91dd3783c1b890efd31a5ae54e07ae1230c660a02201c83fa1b72a5b72e4335f479b199b25f872541a3c54438bac982abd687ccf7e547304502203a1698191ada519fcebccf7c01583d55f87cab9cdf4cef63d6f2a32d35372321022100d701dd2185a629933304e6f15b484a09c697a3e1f62de2778fd41365fc745ca03f6052899fdbc39ba1ad975ac9ed0ffde749b9eab26c83b9b71947a3daf25decd6f047b0a9a11c266946304402207dceecbac897aca7f9e0d0d5248727a6c5eef55db9e90a415091e5577c79fad7022076adc52eae01dd98217ed6447b15b6a527ebf39a320bec786140ec3e79ca33282102280496b0d59ada787b22612fb7f2d65577a2a675e11f8a9a37e03d4d06f10ccdff2b3477043257df952e50f762227c110098b172d8fcc0d82acf95aa683deb9dd7d27584349302d46a473045022013a0cbbe5252da8b7419a7f86293b97f9ca3ddb73812479ba1484db3c4df425b022100a48a40425aa27050a19d53bc716cc3dbb8466d418a3d99d3fdccf7ed0cc4cc0f2103e81e54491f99a47c1f871330a04a2e5c630fbf31228450a2ca007923d8f3a5b40550c48ad6aab7f479ca75d22e70687c3dfb646454f99f61aa9457b7d0340aadc762520e26fb587e00fd6f42c20000000000000000000000000000000000000000000000000000000000000000ffffffff00ffffffffff686db48d1714d2b3cc6482deb80e4856340e701d59639b8bfa8ae5cf1168db4063cdc44746304402200bb9b6578e85045c79520992f02269db0a0ef7b298ebb0d936190ee1bd9c641b02206c72ca442b05ad48fd786cfeb37c136be50e6bef37c36093f81376d37447259e2e9dc242027eb066535f37442300dfcda95f05c87fc92321024c7a36caf00dc8ad79abf7e968a85c1695afd307b673fff911e086f319ab06d1ac34bd1167a68f648e03dbce42ef20eb6092512c6fb53484eb06ab97844fd0ab145b3f05fda8d3bc0ed880a788196a47304502210092d6e5266c3c1024dd0166bfd833bf965a1b0fc512dc5a49ef7c89547a8a65610220079bc3f7c335b70f8f3a9c60ace0b8e808a675e599881fbd3fe19bb2cb27603621035f6be693eec96f8c360e2d10299387f5f45927a3e7a465a8ad5e77ece3dae0158bab3212833e8f206c35d553cc575ac4ff0f356ba33df260a36b5e0a83177928e6448db5a6bbcdbd4847304502200493b3a64f6c9199174bfac37e9d3575e56e003b9555c806d70cb9048295882d022100c3f78509e167f6890efe9da81c99a5f3a9555e488c0fd168e6481d6ab3c2c2f5b2b7a214ab700b18940a7ec257310211c7eceed3b4c35f1daaffd1ec8831a614a18c98a508cdbf8d6b483046022100920e66b6a40a7726332dc0d863b6654097facc828e28630c6d8d92dbb4891a3c022100df80538541f8afc60b9e7428261bf81e2e104bed8d328558cab66102fe418de321032a85eb0fe5d2476824604ae55f3fac1d0775a07aa03f67e0d5d5abb245f5a50fb506600d0165e48bf8adfcef172321022a97166152744edbbfd4070d68cf30a903f3d92b16991a3f1fad34d0bbaeaf9eac21296c14"

    val block = Block(hex)
    block.hex must be(hex)
  }*/

  it must "serialize and deserialie this travis ci failure" in {
    //https://travis-ci.org/bitcoin-s/bitcoin-s-core/builds/385314626#L3142
    val h2b = BitcoinSUtil.decodeHex(_: String)
    val block = Block(
      BlockHeader(Int32(1702154674), DoubleSha256Digest("deab7362c460f3af3b667b139d1975571044526a406e945ee9c0e4e30f88684b"),
        DoubleSha256Digest("8c0cb73b9cfdb229200c2969181b7246dbddb0197438c6061841d8bd8a02f1a3"),
        UInt32(2721680875L), UInt32(3694387219L), UInt32(3979764161L)),
      List(
        WitnessTransaction(
          Int32(113490575),
          List(
            TransactionInput(
              TransactionOutPoint(DoubleSha256Digest("d8d4fe340b1623086b3579623554d7933d1c610f7b0d7e39d2cfedfcfc9803b8"), UInt32(1143690589)),
              P2PKScriptSignature("474630440220609b02942f8f8c7552ae5f3c9cd78ddbc24d4c962c1f047835fa6e0e494d1f26022076c835b57239c643a0462bd260d2fcfa67903077915af61118c8e04441e33efe"),
              UInt32(3692244737L)),
            TransactionInput(TransactionOutPoint(DoubleSha256Digest("8489c581e37b00c4361d596352f1f16ae48563417e246ac97ebfda34fea052cb"), UInt32(413622685L)), EmptyScriptSignature, UInt32(2245129903L)),
            TransactionInput(
              TransactionOutPoint(DoubleSha256Digest("f595a9a51ce8d3d472d39bbbffb71f3c829aaae6f330c61171ffddc70d1823ca"), UInt32(4211757535L)),
              P2PKHScriptSignature("6a473045022100d83bfee7cb22a61f4f156852383add119e71107f4fb908936dcbc256c875c0c602206d0149c44ac68be25c36e28a7ca5f14ca8e62553aecc7a1b9e4a33257033a1f5210347ff7f5036115ce68bec1d81a3f62f2e793c783de55df761970beef6d6833605"), UInt32(1468279141)),
            TransactionInput(TransactionOutPoint(DoubleSha256Digest("9f32398aa2ec2ba9a4c1a492cba77cbe40f979054cef352879c6fb728a9fe8a6"), UInt32(1884963457)), EmptyScriptSignature, UInt32(3482972665L))),
          List(TransactionOutput(Satoshis(Int64(-6385688028548143174L)), CSVScriptPubKey("24083ffb01017fb4c793b27576a914b9a5e2d7f82a4d67795a0343c9bd9ece0a1e55d688ac"))),
          UInt32(2363768941L),
          TransactionWitness(
            List(
              ScriptWitness(
                List(Nil, h2b("01"),
                  h2b("304502210089cdcaae2481928f0971290880d2c176d9bd3f2903414017050f7bb786541a0102206586eb884e621cf9777539f4f95020ead5a866ea034ad6dd3b27729e8b385fad"),
                  h2b("30450221008e9c7d47ab46f9017798f66abe9bfdaf12d82d4b6591d9f4da1bcc2eaea41f2e02202c6236c4b0722fbd3de3494c5cd51cde214766f7298f2781853289e5ea9ddb52"),
                  h2b("304402206fc147236b367a105fb04f48dab4777462d1ee664af0672cf20b912e16a2092502201c16d96f274389b29b7f0a2cf3b972fe4468bf8ffc67888d98d5defbe61dad80"),
                  h2b("304402207b9648d9287da6101ab165c7f932713e9ab930dddbca6d86096f2840f06bfcfb0220745de4f802617f7400c0b70465eb5fb1389911f9e0e6e505c6577744459fb168"),
                  h2b("3045022100eaf3c55f92d176e708a970e8f87b08fc6f50a1c9c86d60f3a8ba0607bc9425e40220407e3cb998a5f405026d1aeaf598c6ccd046f263b637b44f91891f394592d612"),
                  h2b("304502210095aea3322294382a763360669dc0aa5f94312fdcdaa8cb63acf5b181d4fd92b002205b69f7373c7e08d88822f2b5cfa9640793ebd3bacc045b4db829cd323c790ee5"),
                  h2b("3045022100c7f40793b4518a46e7331f074dbfb8fea97fb33a78d6d29de9c953ea89923b3202203fef1d7002386ad28ceab72656b13c7f943feb94478e6af1ca33ccba7a90f565"),
                  h2b("304402204bf769ec448b8fffbb1aa869e27fa5571afcedfe5b6b883b1b5c5dbe7049c64302201bdca453167065a2bb35baa325af6ef472249af23cffc000f55504f80ad2baa4"),
                  h2b("304402202d2d93bca02e911eba0940cf7ea0351ca292cefb3a1ecbece2c0813aff1e79ab022074b9276b148c18081565190a8c09f21017ede9e0c404b58941d5fa9bffd9df58"), Nil)),
              EmptyScriptWitness,
              ScriptWitness(List(
                h2b("02da53af8b115068fbc7f66db1de0d7d50fab5de44d80b1457e1152f9fad931d4c"),
                h2b("304402205998f2c30eb49e03377260c94b6fc145f4e2df8832373c4eb0a7024687635d4d022047c9f3dc7d35542956677410208183303fc2056597300807de76d1d2728f6ad9"))),
              ScriptWitness(List(
                h2b("002103f9faaf2f9e45bc7fb8a8b439791486bdf606400ece3678fb52cde3d088316af851ae"),
                h2b("03e7060c5a4a83289ef10230f65e795d2fac683cfadd1cad6c18f96d2aff8dd7bd"),
                h2b("3045022100fb02972aa67e3588c81f8beb44a318199b208b3959b05108eeb8bc27cb6e267602205da2707b8303c84905373f14e732e7409746de00c339b792e17d707e924516a9")))))),

        BaseTransaction(Int32(1738718001L), List(),
          List(TransactionOutput(
            Satoshis(Int64(1461444479177248269L)),
            CSVScriptPubKey("2e08374d5a94f018d6a4b2752103d5677b8330fc5529ff38f7977fc66b0932a4b517a7440a9281bf3859048774d1ac"))),
          UInt32(1614462158L)),

        BaseTransaction(
          Int32(270457724),
          List(TransactionInput(
            TransactionOutPoint(DoubleSha256Digest("f68b8dff63b2a6fda07eaf1e8346afb9e12eb89cfbe39376817960c269ffedcf"), UInt32(949607868)),
            P2SHScriptSignature("fd7e02473045022100b00ae5a0697595bab82208e0f3b8c5ede59d53cf2dd41e8eefcb03b8a8ede1a1022063a8a738d9841c47aaaed0593fe731cc04a0692ac5eee622d2c89fa1d4f4bbc2232103ef79bc6382047b0a0e5f498e8fa6c1e0c7644dffa95c65816edf19c36288f3a5ac24081e7dd95f11d0dd74b27576a9142cc82db79738061377c03912856819d0446e8f3288ac4dea01084b9a49a6fe05aec1b275562102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d2102f00a76ec708f7ff1a7a4af882c013ff9c3ac0819daaad16870757690d3ff952d5eae"), UInt32(2881938328L))),
          List(
            TransactionOutput(Satoshis(Int64(-291475844515814906L)), EmptyScriptPubKey),
            TransactionOutput(Satoshis(Int64(4235396004392933122L)), WitnessCommitment("266a24aa21a9ed4da6259ad6e8b7757ea4b0ece4fb1ce8acd7d692f0215ecd76d9cc5a0b9b14aa")),
            TransactionOutput(Satoshis(Int64(-747768635414018409L)), MultiSignatureScriptPubKey("030000ae")),
            TransactionOutput(Satoshis(Int64(-4067877222624965319L)), EscrowTimeoutScriptPubKey("906352210231e81f95ef6e9ae520c5c9fe852c65a58293f3cfa89b99c350ae5588aaffe08d210231e81f95ef6e9ae520c5c9fe852c65a58293f3cfa89b99c350ae5588aaffe08d210231e81f95ef6e9ae520c5c9fe852c65a58293f3cfa89b99c350ae5588aaffe08d53ae670868af66e73db60f33b27576a914b9d15f54bf65291c50b93dbad363a546fd77348388ac68"))), UInt32(3770465129L)),

        WitnessTransaction(
          version = Int32(315296677L),
          inputs = List(
            TransactionInput(TransactionOutPoint(DoubleSha256Digest("22df6bfe3d798a7666f22bbe16cf0d4ddb803854165d1892fe9a9421f062084d"), UInt32(1880576646L)), P2PKHScriptSignature("6a473045022100ac3c9aa7eca3c4aabc5d99ef7aad720dff61b9c8fd488c5dca4a2622a2989d00022045c300464ffc5952f338539953d976a18f98447b260347161c143d53e528b30721034ad77bbe794130918d7531ac94a8b9fda0cb243fd5f180f1a406a5563a2b5e56"), UInt32(1869948412L)),
            TransactionInput(TransactionOutPoint(DoubleSha256Digest("e56c4da3fcfdf15d44cb6bd4fc3ab9d3887afc2062914781a452e7d4dbe17987"), UInt32(2588065991L)), EmptyScriptSignature, UInt32(931621001L)),
            TransactionInput(TransactionOutPoint(DoubleSha256Digest("6bcdb3ef4638b51fd834018c13cfda2c6489a475a01cb3cef15296cd06b51235"), UInt32(215702161L)), P2PKHScriptSignature("6a473045022100ccc11e726f95bef6b5e1b4675b06cafa8c0123ed2f39dfb8ca6c74ecb8d874e80220597371cdad620d90e2f45238c3c61035fdf793722eeeb8e34d90ac54265c56ea21035512856bc877f511059560355d998dd2bb84b899136b047a58352c8a9774a984"), UInt32(2035404185L))),
          outputs = List(),
          lockTime = UInt32(252611683L),
          witness = TransactionWitness(List(
            ScriptWitness(List(h2b("76a914dbd4a2e1eb29beacfefc14e8096abd31664c7cad88ac"), h2b("5a14253aad5036a0104c800075309e26fad132cfe3c8"))),
            EmptyScriptWitness, EmptyScriptWitness))),

        WitnessTransaction(Int32(187642592L), List(
          CoinbaseInput(P2PKScriptSignature("4746304402202ce1111c19845e3a5fa48986d14301b6b07714f123505f8c8c54a033609365050220323c92fc88b7c4c82f63d114074810b0f66eb7964bfcefbd8d93a5e7259eb5c7")),
          CoinbaseInput(EscrowTimeoutScriptSignature("fdae01004630440220071b19ca403591e0a61c7df0ef157b72e1f3e27b65f51a6c213ef170a13f7e6002204c95173e989f6eee7860e94963c3a36bd02518115ee1e2dd9d7deb9b2532a96246304402203bbee295b6406afd55007c6ae932338bdfc30753a5fca68563439d88abaff4cd02204518216aa64fba23488c3ecb7dfc4944676c7a0bc785a0e3111ff29c7f5a4848473045022100d668340252f6336b985b8d99200bfdd61afacc3c2fc6674c4f48499d9fb534a302204c03f0e779148e8e466ab34e32e0df50696f3e323ed7085564c25a4140ecfb3e463044022073744cff396bfaae0abd787eb3f4785637d7668eec62ec80b6d7fdfce599af1502205dfa004ce2c8e4c6dd3cd1b31a99c6205b64c17084f61d85cabc43f5c3b4c31d473045022100e7f1b7e22788f17d0ec6a3f2c68b779086d3cfc9231c15ef5708606f3bd8411202204301afe1209afaf73d56c9c5758506bb9fb585c7474939fce835f5de7406c50c46304402207d77cd0d8d46c86625585c3e205f257788e0226f4a3230eaad77875deb420fbe02201f90e51ff0533676a77d32fdbb224f8a99772848b2deae057b0a25ab718dddb051")),
          CoinbaseInput(EscrowTimeoutScriptSignature("18160014c16ef6e9d9cf069835db3ad00defa43bfcab7ae200")),
          TransactionInput(
            TransactionOutPoint(DoubleSha256Digest("87f6c324278f1eca0b1b2985fd9141b89fdcbecfbfab26652c68cf192e42ec9f"), UInt32(2699355796L)),
            P2SHScriptSignature("17160014566809494125efe3244310588d63e65e4e8625d8"), UInt32(1437502814L)),
          TransactionInput(
            TransactionOutPoint(DoubleSha256Digest("9d2a7a815f421c4f1f9aa5ae00dff0af108278f2110c4bfc742107cbc6601861"), UInt32(822076758L)),
            P2PKHScriptSignature("6a47304502210092c94f9d25edb5cb7c60e127e07661780625b4d53bf712f3f13bcb89bb4a17160220306f8756192fd2c884e463bb1b13908600415d3827c3977e08e180ac5ce824c82103231199bcdd2c2d6289df05f20d56987d443f2fc53778662363ffbb70fc472716"), UInt32(4259908627L))),
          List(
            TransactionOutput(Satoshis(Int64(1724132477859763854L)), P2WPKHWitnessSPKV0("160014aa5373effc83b686379a794a849f117632104467")),
            TransactionOutput(Satoshis(Int64(780746907748730823L)), WitnessCommitment("266a24aa21a9ede51e88cd69fc789d0da9ffc666e6dad73816ed7244eaeae71765531b327dadd6")),
            TransactionOutput(Satoshis(Int64(-4191472121767118350L)), CLTVScriptPubKey("2e08fa9670dc084882a2b1752102ea9ac2a181d6bd05d63284a36ecb08ceb185a3983cc72b289fa66dc63b788aaaac")),
            TransactionOutput(Satoshis(Int64(7573945953573723808L)), P2WPKHWitnessSPKV0("160014e551e98b5340dacca3706fabe440180e3ede83ba")),
            TransactionOutput(Satoshis(Int64(-951272898134594819L)), P2PKScriptPubKey("2321031f8aa8d307587fa215e7c83fd124a5233402d26cbe1dde38850e79b29c7c6df5ac"))), UInt32(632316359),
          TransactionWitness(List(EmptyScriptWitness, ScriptWitness(
            List(
              h2b("02c259790e90a91e5fda36ef24e11ce4d4ccb7f86766c7e6ee922d308c1be56c67"),
              h2b("30440220472212fd49cd38503dedc3e7f0ec7ce8a7bc44d9f0feb63434dd00ec81728cf902202fe4ba1cf1057ff83cb4cb8edc2284b9f6f2d5506b820f3532f36f3bed64111c"))),
            ScriptWitness(List(
              h2b("512103d1d63150757020fa412e8f359e902c13c380338899a9c42a869f70a4e33440582103d1d63150757020fa412e8f359e902c13c380338899a9c42a869f70a4e33440582103d1d63150757020fa412e8f359e902c13c380338899a9c42a869f70a4e33440582103d1d63150757020fa412e8f359e902c13c380338899a9c42a869f70a4e334405854ae"),
              Nil,
              h2b("0880aae2a671ef7a82b27576a9143f0a39796f2ad33ee1f15c925c68a1b7644940bf88ac"),
              h2b("028850f7c1217a4501e6f3b7b0271f4a0884b6d9ec406c14c3742b221319df9f3a"),
              h2b("3044022100ccd7060ddc7eeddf191fe58c9bfd67b258c3726b2ad988df89126208f9b04b89021f78a67ff7fa4fff0252469a7bf1684d0713da53a64d85ce899158355de3f7d9"))),
            ScriptWitness(
              List(
                h2b("02c259790e90a91e5fda36ef24e11ce4d4ccb7f86766c7e6ee922d308c1be56c67"),
                h2b("3045022100b4aad9315c8c59b0ae861014a9f424d6adcfcdfe6295181c8d4042486a9edfde0220586c629413b4abdc235cd4d79323384eaa5b9af585b70ea08c89ab93fc16a371"))),
            EmptyScriptWitness))) /* ,

                BaseTransaction(
                  Int32(-479681110),
                  List(
                    TransactionInput(
                      TransactionOutPoint(DoubleSha256Digest("24c32f4b6e3680eb0a1c851bb8dced053709a40019653a8a801151a4f10a7e36"), UInt32(1948832136)),
                      MultiSignatureScriptSignature("fd680100463044022018a077a34c1af895f9b2513a3d8990ead518dd139e9550f183a9df8d4c6ab9680220545495449abc82c8f22e587e1e1b6c946489aaeaf0f15d9b4a466d8333304a1c473045022100f69aa24e22e56c107fe0c467d94228ce32644f2de92239b9e1fc8f4d55951acb02207936776ab7b035d94d3fb2e19d9ef3093acbbf3ae91ec97b2991089bf84775b7473045022100ad13564310ef64e23a6a39e5bfa9fdfeb8a378b0f1ff4684a5fe9f22f399131d0220642ffd1b5a1a0c3ba305f6abd9153d0ba0489859b016af4e691a72af93ce3f3e473045022100e01830e91c3cee80523db7491d6a028b73c93e53aacd8b7e0088e3d0a4cb4c30022068b068e6f1b14dee1152640effe73343f484a84a06b37a5e62905502ad3e79a4473045022100df50dce1be87b15af1de24246b519038c3c96c83a33cc0082e7cf2cc9161411402202d241543111960aaf06db781a11761dc795793935c1663314e483d0130fb9cd7"), UInt32(2749804980L)),
                    TransactionInput(
                      TransactionOutPoint(DoubleSha256Digest("a9a4d5fe7b475063e6a80ff56e831b82940aa161311d7bb3ff7d8109134462d4"), UInt32(2942421187L)),
                      MultiSignatureScriptSignature("fd090500473045022100f2ff2c2b7d51e415f06413dbbf9c6c4fb2c3cc791512358690f6ab810787de2102203c0d24478e7ba160f93e9c567c072ca426c3557996f505c1a69a7467907d359e473045022100dbd32b4ca490f619042c09b02ce3844d6e34da4f5e6b6a84bdc37cc7406a7bfa022004638e36c28a6852f75bb83cf263613bcec8747b9de303b33006f7fb05ab045046304402201bfd69a7ffdb165b5bf063fc905b6ebe8f521a86596a4962741f23468d95384f0220111f9315844743c3e92e7566727a255cd567d070a7da9b2b8250c59e51c0fc6f4730450221008e5fc59ed075f8b18ce18756c0f40f61e4a5503bc0d83f6b284bc008c408b45a022077c66839c1ec1295477d8b02798d21c21182d2db76a03e77494bc665fb2b254246304402203b55659d3e470f59270cee4c7512eaf1d41a6c8a310fb655423bc0e1c239b31602200507015abd0f344667b36f81fe953c7f8a730d97972d0e5070b3144a7de4906d473045022100e80dfa023274e408d50d06c4f6582a0c84ebe9f87c3bd956c526c434a168554602206027ba877c9c90039fa6c7ec66aa26dcc7ad5e041019f7733cf077bb537bde56473045022100914813e2a8e675dd86199db2555a00c9e565c499e733a83d4e4aa0572800f7cb02206df574e578fdec18c2c23a27fd729527630d600898fb0f962634b16a2da642ec4630440220370bf7c4f213d07f01bea0e43b015204b6d3b865475c81a316e37554783ab4be02200e9917a4cf11e4dbf43c2be760e2208083046df3c79948e928de1c1badf0ad9b473045022100acd091f11f4ef0f21039d35b04ce91aa7e2e527b3ce28c035cee485a581a086402201722f7987a03962427f0acda3fe733478d4c60e130ad64b439e301f3436b7b72473045022100f7bc236c1ae27f893a68acb7f7950d19e2d3b92c1e904ddd40621f11afed6efd022062e4267840d0aab4fe07022865497d70d867c10be9973d7c6db760d5d5d00dbc473045022100dc270ce44be63a3ff7352b244d081a69b57b59a3215d218f0fdb46cf17e795810220422d20cf767a3097b5ca80b08a99f26b6338baa0e06ac0a5f0893774437c530c46304402203a8b86de6b8dd29997876093e1f3fa6cce456dde324a2cf4f6bba7924aeec24d02207e59258762a4dc04e7c2775d973b5df50d0cf2a0535cbb2946a84e8576b5d957473045022100a0ea932f5748b9a38ca308e3a9ac78bc004e6ff6ac0174d354016a46e99ad18a02204e2a00285fad5ef0ea537742ae507100e6bdb4cff24b092bb643a73c38aaa8f246304402200dad833eb58b1a1e72978ff1cc127e722a8198950f90eee378124f6fd456f5330220027d7c5c327f02dfc089119444b84d960584139bdf1ee06aed99c58ad775d1d1463044022027039290533030eed05f282c825957e85862a4c40df46766cc98f9284eb07fb4022011594cf865aaef4a144ac75e13cb653fe693bcdaa0cf07b2b52ce756cfd1d8ef473045022100af0ded853ea879030424e304e2cc5dc36fe79defe760995316be5038172b16a9022003ae5e4c0d56999ddcf457aa879edf1fef2b9116c8272c6c02e46230f1e3911f46304402206b4427b1547ceb46138712af42a3171ebde0864e5adf7a01e07da68e422d57b002205856719ccc06cef909f9d48fb57c4795387c065372d29decb441359d27fa6a5f463044022021f89b1e8f0cf4e2d7b7b498985b5425c8e868be74f0ad07c1de5387a93c2c4802203e1833f60343dd1559c5688cb69d4da89ffbfcb4f1a45bd89bb4602acc20b301"), UInt32(2822395923L)),
                    TransactionInput(
                      TransactionOutPoint(DoubleSha256Digest("d986f337176b85d70d50a4e54fdf2e65737b319f793d4485ecc557603f5f64b4"), UInt32(2776778948L)),
                      EscrowTimeoutScriptSignature("fdf50100463044022032e6a55d57c1d06dfcd8ae8b61ae18edeb50c3e42303b8272b4720e3bb3e6b4402207977ab55e3e17776887cb7f98a28e86e681f3a2e2f1131732cbe8b69e9addf2f46304402200c7c585b0216d67aeae6dc9da8473fdf6cbf28b2020ad9ae72a8077e28d28b6d02205cf454f73e0c4f34c54cc6b5c5df760c9e0d12f216ebaba64021dd43fb27743b4630440220673f1162125e72049f8c6f97810c49eccd7881b7baacbcab2ca2967e44b3d33402206ed3cb89bacadc1a7ca892dfb3c2af1a3eb30b6d83b07a3a772d035582bbc46c473045022100cbc6dcd3cddc85e5e713c07a9cf35b8fb5c418e2934f98fee35e81915bf43353022040e0ac1e718107b4c0b8ef50ba1994d6965db38c15a806374ffee1e1ef1b44d746304402207efac14d36c6568b03e271f8dbab2f3ec0339e1dfcdbbd4f3fee00f742040b3202202eadd5ae0aff60b5d06d1670058920d9bc19ebb9cafb2bd84ebca0c4de67686a46304402207f40e2d81eff6182c574a05a3db1b7b3072296f25e7396a8a7de3707c9738d81022070b8d908ace5a1ed1b64b1bf28e9d3c6147746732a75e751da708caed922823f473045022100b90eb8ee5796f1ff4783e3399f48388bc6bca8111e7e9aef7ad34a21b07f0d5f022077a0df3bb3d6518eac6b2e40b64ed5f1bf5ffde1a7200fd30d76e38117d4c48651"), UInt32(857902082L))),
                  List(),
                  UInt32(1235115183L))*/
      ))

    //notice how this base tranaction has the witness tx marker and flag bytes set
    //31bba267 0001 0dce50c0021848142e08374d5a94f018d6a4b2752103d5677b8330fc5529ff38f7977fc66b0932a4b517a7440a9281bf3859048774d1accebc3a60
    val btx = BaseTransaction(Int32(1738718001L), List(),
      List(TransactionOutput(
        Satoshis(Int64(1461444479177248269L)),
        CSVScriptPubKey("2e08374d5a94f018d6a4b2752103d5677b8330fc5529ff38f7977fc66b0932a4b517a7440a9281bf3859048774d1ac"))),
      UInt32(1614462158L))

    val outputHex = BitcoinSUtil.encodeHex(
      RawSerializerHelper.writeCmpctSizeUInt[TransactionOutput](
        btx.outputs,
        { o: TransactionOutput => o.bytes }))
    logger.info(s"btx.hex ${btx.hex}")
    logger.info(s"btx.outputs.hex ${outputHex}")
    val wtx = WitnessTransaction.fromHex(btx.hex)
    wtx.hex must be(btx.hex)
    /*    logger.debug(s"block.transactions : ${block.transactions}")
    val b = Block(block.hex)

    b.hex must be(block.hex)

    b.transactions.map(tx => logger.debug(s"tx $tx\n"))*/

  }
}
