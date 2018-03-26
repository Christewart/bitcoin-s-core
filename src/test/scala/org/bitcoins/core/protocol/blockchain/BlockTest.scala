package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.script.RawScriptSignatureParser
import org.bitcoins.core.util.BitcoinSLogger
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 7/15/16.
 */
class BlockTest extends FlatSpec with MustMatchers {

  "Block" must "deserialize and serialize a block with two txs" in {
    val hex = "b0aa0dced271237b5badb4365c33bc42b8fdb05c3defa03e75dcae425f42ddd210d69b8695e0f3cdf1eb9f56d0daa3c1e384204faa20792217d3103217773f4177b6b0f067079ac1ee733798f3bef9a0" +
      "02eb9a240f06a4c916eb2e865ee48befdea6840273bbceb3546c0618d03b420c16fa10e80571bc81fb77fd9f05004730450220769b7dc2d74accb2f3f0c0d360260905129d72ed097ff5fe53072191c7303bc7022100d01c6d97c9fd9887d898349b3f12091f1233d9db5e9facbcf8599efb9db259574730450220751d9e59967c00b706f19642f93672c70d47141ab2516e33c8c20f206e6838bb022100bde2f1d87ec1b2a2289127ba2841cdc3c6dcf65c3c2fe3f74b53535dd8170f46483046022100d3e34631872344a41b4a1b1d2c63b484aeaf6edc6fc5d836fbffbcbbd5e9121002210094e11649a9da76020ad4188f7a5f588f01cab0cb8d6591c122ae0db03fa03a6d473045022100bc41ad4c6af0d7e3d2a61a3d7fdfe7116927bd563b1112a3582261e8a9aa9b44022041c58ec9163d88c158475630493c62079c1740727a44c6ca862b07f7d273022e46304402207aa8d6e05a81e2f17840e55f6ca96ded6abf13ed970c5ef6a5a7f1af1d9a552002201b4898853632e1c12ea2de3b93e257e1d06fee9f01990375383da5aa88c0eb7946304402205f055b0f557bab39f4ce539075691667e20cd5ac64752320ea5a12caa03db2b6022053cfb6887e0068568388f986023f0e08b214a7e598d7c71192ad81dbd6b1218b473045022004d31129c86aa93ea0bd4e43e1daa1101ccfc065e5e5a98dcaa5de2cf663f2bf022100bc3208e7afd0682c544c692c9f7616d82feb686808b05b0c1d3f98cc38bdac0e473045022100f0385c6ca49e5198befa6663c30af80f873449d553f9fbea188167b181d0b15d022059641c26eeb3c7e0af7fd2e2edd9b1a3db2946497ac21ca71cfbcfec94d69fc9473045022000dbb499b48c8c2026128b78af3e5b9b6e4ff8e505a8b6effa2464902d2635a2022100b205e57aa35a944d7d8d001a3f69640582f7dc9e6703fa69ad37c210018dcc8c473045022100cb31ef46254466086aca276d8d46a3ffa8035a042751242804df36d0151adacd02201743ac1e0f9ee3a85129e56a9860ab612bf2c1e0afb80d4ddbd49521401f579c4830460221009f880f98cb0566027fb10e38feb7a63c875128e129ea5565fba2ad8db25ba517022100ff49ed12c73bb4d90b57de30c623906e92ccbdd62dad21c88fa5d7b16dae0854473045022100ab3d3db5204cbf011c2f9fffd687ee1fd026ddd3c32e22d91a37b328e1cff68402202b93cd46eb3473aec0e422b89644b6a65803f23c3d1de942b03e8e5fef02c6fb483046022100a62f8e18cf530cf2798a6a3ece89b22a0ad68f787c0b4ea5fbaa1f1dc7c6af7102210085de71bf5c6fead1cebf903b0e63bd4b0502b4784ebc3c56122bd56a8e5a949f47304502201ef3f95b628b1d11ede235b91699f205c20583d974131044b1c28c862b767fa9022100f79ac57c6824f232da1b38135a07f62b8e93b571bcce0e68faa51ebeccd5b76d473045022100dd5fffb857af413ac80d14f791a9afbf0b966130630e8bf385c2da551c41617502207173e35682194bf079f746a4cf66fd58b9435f0fb59d2ddb5a9a481589e2b6d647304502205db8ddf57d042f4fc6496d0e9816695ea343894b9fbc23bba204e276b77ea751022100fee4589a862f469f8d8bce588fe2f8a22d787dab01fa64a97abc25597e340b7a46304402202a85eefb6d150d463c55aae8d0686f654f8086f0a310b52b7e8cb00f26af2ca102201987cb08cbeca18d893ce4eb4c0fcfe7a6a1905bfe0b60da035a37cb4f8423ef46304402201b5c4cfc6c3d97bbd63a2ddcab88861d697e6e069f474da00d0b95f41f6b4caa0220545a2645531cdaf51e3bd8bc5a526a5d821b1d8d01e72f1dc49642bb438fd3e6463044022051b677c9cb95804e8879f9ece91dd3783c1b890efd31a5ae54e07ae1230c660a02201c83fa1b72a5b72e4335f479b199b25f872541a3c54438bac982abd687ccf7e547304502203a1698191ada519fcebccf7c01583d55f87cab9cdf4cef63d6f2a32d35372321022100d701dd2185a629933304e6f15b484a09c697a3e1f62de2778fd41365fc745ca03f6052899fdbc39ba1ad975ac9ed0ffde749b9eab26c83b9b71947a3daf25decd6f047b0a9a11c266946304402207dceecbac897aca7f9e0d0d5248727a6c5eef55db9e90a415091e5577c79fad7022076adc52eae01dd98217ed6447b15b6a527ebf39a320bec786140ec3e79ca33282102280496b0d59ada787b22612fb7f2d65577a2a675e11f8a9a37e03d4d06f10ccdff2b3477043257df952e50f762227c110098b172d8fcc0d82acf95aa683deb9dd7d27584349302d46a473045022013a0cbbe5252da8b7419a7f86293b97f9ca3ddb73812479ba1484db3c4df425b022100a48a40425aa27050a19d53bc716cc3dbb8466d418a3d99d3fdccf7ed0cc4cc0f2103e81e54491f99a47c1f871330a04a2e5c630fbf31228450a2ca007923d8f3a5b40550c48ad6aab7f479ca75d22e70687c3dfb646454f99f61aa9457b7d0340aadc762520e26fb587e00fd6f42c20000000000000000000000000000000000000000000000000000000000000000ffffffff00ffffffffff686db48d1714d2b3cc6482deb80e4856340e701d59639b8bfa8ae5cf1168db4063cdc44746304402200bb9b6578e85045c79520992f02269db0a0ef7b298ebb0d936190ee1bd9c641b02206c72ca442b05ad48fd786cfeb37c136be50e6bef37c36093f81376d37447259e2e9dc242027eb066535f37442300dfcda95f05c87fc92321024c7a36caf00dc8ad79abf7e968a85c1695afd307b673fff911e086f319ab06d1ac34bd1167a68f648e03dbce42ef20eb6092512c6fb53484eb06ab97844fd0ab145b3f05fda8d3bc0ed880a788196a47304502210092d6e5266c3c1024dd0166bfd833bf965a1b0fc512dc5a49ef7c89547a8a65610220079bc3f7c335b70f8f3a9c60ace0b8e808a675e599881fbd3fe19bb2cb27603621035f6be693eec96f8c360e2d10299387f5f45927a3e7a465a8ad5e77ece3dae0158bab3212833e8f206c35d553cc575ac4ff0f356ba33df260a36b5e0a83177928e6448db5a6bbcdbd4847304502200493b3a64f6c9199174bfac37e9d3575e56e003b9555c806d70cb9048295882d022100c3f78509e167f6890efe9da81c99a5f3a9555e488c0fd168e6481d6ab3c2c2f5b2b7a214ab700b18940a7ec257310211c7eceed3b4c35f1daaffd1ec8831a614a18c98a508cdbf8d6b483046022100920e66b6a40a7726332dc0d863b6654097facc828e28630c6d8d92dbb4891a3c022100df80538541f8afc60b9e7428261bf81e2e104bed8d328558cab66102fe418de321032a85eb0fe5d2476824604ae55f3fac1d0775a07aa03f67e0d5d5abb245f5a50fb506600d0165e48bf8adfcef172321022a97166152744edbbfd4070d68cf30a903f3d92b16991a3f1fad34d0bbaeaf9eac21296c14"

    val block = Block(hex)
    block.hex must be(hex)
  }
}
