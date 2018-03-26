package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.currency.{ Bitcoins, Satoshis }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.protocol.transaction.ZcashTransaction
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by str4d on 3/26/18.
 */
class RawZcashTransactionParserTest extends FlatSpec with MustMatchers {
  //txid 4070553dbcd801788e381692c8cf78cea3ed690ec93b359c5d91848dcc4e3a6c on mainnet
  val rawTxV1 = "0100000001a893a0260254d946c167489313dff73788296f9962d0b40e1cde113fe30fbff3420000006a47304402205c9ae87f0d58c7a22119c23a5a021d2de7c77ba33a89a1ec6bdc18a2c9f9e896022021abb54cba4ad854a7bf8e9f073f1e120ec841d8fcec6ebbc70fc7c4befdd7ed012102fed901cbe669b4ec65011f43f223fcf6efcf2316b2f3896318a9854e304d3916ffffffff0217f41400000000001976a914102f2ce538dcc3ccd1e268301abfca661c8f7b8c88acf5d39300000000001976a914bb30b068e30968de410b88d3e9b59f7fe012185688ac00000000"
  //txid bf1706af01f60f5347eac2291358def1467cae709223018c0bfff38732390c67 on mainnet
  val rawTxV2 = "020000000130838964c8dc456e2acd1eb2b30f33474e1dd4629476eaaddbcea93a8a319353000000006a47304402204f50c9c167ea4c96170e4967879487f372a9d40f8304615bc0431ea0cadd251402204deb2ca4d305aad404b5f50200b5186f224ab4bcb33d982d487102954af49a180121032806b2a6a5bd7c5567c5dd01660a0e918491b65c114a366eaa521c31ba412fffffffffff00000000000100d3b53b0000000000000000000000003468aa409524ea3365c4a95dea8cdf614ddc71193f1794d17d7a9fff50f1721a5102bbc88f1ea109ec1fd73b0181756cb8cab94fe748bb1f70bea435d756a918a00159aeaa981faeb7d9dae2f7cf372d3e07a8563c4cbc4b5666631c4984802cb96e065bf2e95fa0b5a4d0e48aadf116ea3e82bd79c71539a5dc0ba5405753109eb2913a136da76a29f65aa7cf388033fb5fb06cfad6f45d224e47466b680f63a91aa1b69a8f09d103fb6515b28bd38a6978ee1e9559f9bf757f37ccaa696a6d9ec328c46ae518c30cbb6520a2e6c9ed488e50005287146b44b769419f8f6734fd2fe082b75bf110a81ec79763c955041bac903dad4ec217fdfd708dffe4abb69a1335e553fcbe12dc3a1d2ad1569e029637ff052f262d201fc56b203235ab54021d83d73e05eb938a1645faf71f8320d89af20cbeab5607944b2343ad23efeb860221aaa9be2f9a80a8e9614405c7a8c735843c0611c2a5ff70ef200d450491ea4f0a03fd206593e047a2f955b998437a553119098600c02d43ff6264e0b4a45688bebcdd94921ab0da380ff9fbbd0987739ec2e99fdf56c51005d520450542e574f5030966a511ed982679e9f12a0f5a72ff005e6af4a4c7c94bb2ce3da94ca05437a102070e4c059e8d70f3f48534444c3139d469f6ee74f5a3575a09d86f430abc551b032b3899e40abde4623927023a83fb05f5b196671982a1a322bd6f8ada054249ed031a5e1bff4f196e923a129f0cf2f4d06c63b53c90e447d2043a3dc62466513332021aabf8470de83b8706b381e0a3953e72c480b7743af5d7da685e3b8d1f5d37c7101afff2ef754c94e0bd3fec7fb47f0c011ddf04e3be84991d6c17b80fedd2f8318013bd95abd02dc49904390dbb7ee9449a66bd7ea26b8ea54794dea40eade0cd9aa83455ac2052c18e8d7149f308a8976b2282022f0671f83059411d51b5231dba09b3f109baedb94a6221a64361788a62cb0c8eb72efa5b9b2c7e5a86e7a1f576814a3ef236cd692bd7c12051e9e619fb4b844646599db84c9eba55a35b0bf46f51f81ff7ca3b3667e93bdbd6e50ca87338fd63aff58d81411b499e38f6f3da7c13e5f8852a3d7276090d6764b85465bc7508fb930a1f6fa8e404509274b4fdb8fdf3bb7937459370d2405b2c285b3c238dfa63272fc197075622dabe1b4df223488488caf973686eb289eac33d6438bca5fd78b8eaf6249f8924c72d070fd8a53d8be3d63e4a532cb6b430028fee4bba7ca4f0cc42b13cd4a89e724ab578648f3b56a9a942bc0421030dc7c8a513ed636383e9c574ba154a1495478c0a99d3b03fbb335df62581b7d55861bd91d71b49af2434ce5ae824a826f7a5d4e44d966c2b6f0473e91d036c05c634b7e927d83d151b120bcd0569471eee83e2c9ed3078d4af143c4c74392fcfa5d3718740a8b81bac578bcf2fcf8d637dfd020a9273deae4302772a997fca6a74fc8a55109b5d94df2826b8428d4c718b7a952ad4420a289a5f759f41276355c49f66300826e35b557cc5935d4ec3c9eeb5e0910b583444d8f450b01acc8d39328cf2727c7fe650ed14567d769f48f612a9dca28623ff94478f3e5b5b5080414f943d09337d97ed104e1695ac8a73753aa30b5d584b685621d9316377e782d1a6d4b3d9e93702b263bf088fcc6144e4ba8776944083da9ebcc50a16c79136666711e653ffef09cf12561b8830cc4fec19942721343cf774a7eadd8a825494970d8365ed9b26813b4042fdb8fe08d99ff046552d31d44ed9211a7b034b3630125bc03d3a5b9416c399493bd139dd98648a0d02a714f713b9db69346dd6060758a4010a7e859bb32f4ee7af30e1c809e2d988a477b2e1a2e4832ff07548edec9283e8f9b8f7b2a67424fb58f91a9ed8b2bfe93e67f83ae9590b579845f8b367ed82dd7f88935f31ef4db3af11b03e3d20a207e4e37f43a15846244922e3dbed2ffb481cc33976055cf04d8147e525b3afd0c4f16f017bc728af8113906eafd366804b4ee63e60d84a6d65ce6732908a80c5cfd7bd7eff6cd13e46907dcbd06b6bc66428d749943bf1fb188668ff141a7257287f9c5211eb2b3f7c2f848ab0981d9f9c514adf90dce5d0aa3facfb27ee43690da1bbc1f828addd59c8d4cb139d848efc2b9ef8e6c6283c12f70f0405139501ec007eb17ae3c1d5bee74603171fcc02003b029ccf4cb6515f8358c0a5903c25374d58b872bcb9b88b4ffdfc7665105db80582ff2a34dae28b250a303896f52442e1c84f8ff3b43ceba0b48a1f868bab48760c77b41cdac6b4427f6dc6b120b8c589e1bff5c38b01e106728418cd0d0940701c30e3aff1b674b2a5c6c3bceb33e9a77071653d92a4b1a5e6d97ba366657db65fad2779a837a226a72bbe3fb540a0ebe1ade1287214a0b748d5ae3983fd17ca33f5f1f5be0fa7bb2da5b75f3a877bde2b2341a3b08781e7f7b7fcbafab3371f87323c37f1a74fd3898d880263bb329aca5357637bb4c468f74de3844e333bf4ab5e20a1f5b3a62ad932d7f80edb569fda18824b54b12417febac3e2feb1e784845b8632e76bb723e0485428662b787be701cd795b32716b135e059fe9cedcca67b05ddc6a6bdd74a0d70d094531858a4009edafc88aa6b760d22db6d720b4fa21a6630f"
  //txid ec6711f8250e1f7972789c937220fc1171e9e93052fc6f0e8e4955f6b5a47530 generated on regtest
  val rawTxV3 = "030000807082c403053378920ef8c26ea56e7f5a9de7a203c6fb9847fbf8e383c8768ae323348e7467000000006a47304402206bbcb28432c90b4857c217639e24745a04a571727385410925da6dfe83c9f24902204e9ce96cf04775e232116f654d19a5d9bb5b96a43916eedc2dce07c8d165f98f012103517f71860923ddae67d327009b14dd950b9fb00fb3abdd7726e9bed3a44ec401ffffffff68305870d8fbbacfe1bac3f02f6e7b631d9d4c9374f3745682dcbe337950ad29000000006a473044022003dbc4282d7a26a66d68a84a2dcfa351ba7a1e20a589d20d92f8fb3a3e759ef6022040bfe363bfb38e265f98cd1f624cc02de486f84d4e138b721261677b49c9c605012103517f71860923ddae67d327009b14dd950b9fb00fb3abdd7726e9bed3a44ec401ffffffff7a48d116bcc0ba9aefa5027b860fc8aac56a11b3832a2d3fcde9ddde08e694c6000000006b483045022100c9865925a2561a81994703d49e129b07c86355092bcb54801864dc3b2a37af5302204e646be31ec7d45a63200b3130912d3448433545bc35b2e95935e86f15e41390012103517f71860923ddae67d327009b14dd950b9fb00fb3abdd7726e9bed3a44ec401ffffffff7d69f7414e171b6cc5edf8b493ff266697fed91f7d73478c700b737bcf6b58f9000000006a473044022042afb65f4f07208430758d5649de32ec1f0c311e5fc78b27ab88e2b625ad6b61022070371785cba8450d8971aca47562c5231b39b1db0175c296dfca73f3854018d8012103517f71860923ddae67d327009b14dd950b9fb00fb3abdd7726e9bed3a44ec401ffffffff8c8e3f82e3000e19422899c5c1f7ab86b026db9abce7360b043a9c9a730a955e000000006b483045022100fec61c3dbb0e99b02d981f53dabe2a701a3f9e1738f8e966fd4c6a66cdf2221a022065a9433651056098a9393407a042051b5ddf31ecec9a08c830786bdaf60ac7c1012103517f71860923ddae67d327009b14dd950b9fb00fb3abdd7726e9bed3a44ec401ffffffff00000000007e00000001f0ca052a010000000000000000000000d7c612c817793191a1e68652121876d6b3bde40f4fa52bc314145ce6e5cdd2596e4264b49873bf31388cd8c9ee3f00837693efd06468528eed57fd77fd608a9c5752c0ed33194a73c9230e642ff34c8bd160fed162785895a4192c112b5dde95dcec953407ec721c3fbf4366a644b9b53c3661b3c2693119b8e3ea23c14bd6c73068eae27faa74b13099b2b7f3e823a1b2a36050ae9e9673170ca7b7e4803e9f0fa3fe1b3b10533e516a6c096cadbf5f0f61ba8b3fbde89969aafa6240483777981988863c8d2f0f24fa03aa8791fbca98bdb6a98b35615bb7ea61683e02e587faa255f0929d4fee7f997ac551d5ee0da82ea9dbb861be2c27e6a7fdd9b6eaacb5df76749a8a6be830126f1c33f167354bbafb4894568699fa3ffa88108495970320967d5859754bb2c1a82f24bf4024a5ac4a63b106c98d5a7eb2651bc36b8e2f022b02b614232c86017b20f63798aecf587fd9cf1c53c54f39e523371de2e6b4f70a02837c7a0ad12c3286b648028f286b5de6729581519660dbfebbfdf0ca39e6ff64675707689ff180664799c3a4c5fb20fc375a9ff475ec411fcca1eb2a243047030034b2a8ddc312399866554b3bdd0394399fd8aa02608f8cbdad1a99fa6aeffe032ebe864da9f4225a096ba338ef98c818823e18c9b32ab7f52857109dcff85f60020909418963470e5c57917370079fad3bd748c12a6b6b0eb697bc8dc46d4ade6802158fafa3b50a61ab0e1fa1221cbd96955ad2b88c3bc208e5ef1fc1c4b7302e8b030d2c842a1e4a49be8e200a63ac5228f6ef0b05770dab9ab15c77ea6a908e4718027213c722c0082cf827464173b6bf20418441dedec23e557ca99df55d53d2c1c198abb4d0a8679be13a49d2176aeebce895c9f00cee7fd037e922c43c6cd9f8e35f080a136fc035900b47e35fe524920d4829d74d5fa9aac9fd9f1dfe43e244265cb5329a61e92d972278ad9556ea36f32de1d62caf9115cdda7338c9439a8440ebc6e01602043c657e8e4a726aef55cbd33acd925a6da9a5d35d05da78b56b37d0cdcf5336c75a4611bdaf226398f215b9922318d17c280fceb39f24d8d86a22a0f399db66ede626df1c8bd4d21f5e78df62712ade348e20e37be1babc1c4572fa9b33c5bb8c0b844349ce0d8002bf133969b026d09ba1ccd1fb995eecc471ffeacece4416b44e45a7454a0b4f96dca6522bd072345cd35b6c92bb0daba9e83ccdedf5ee856fc0da65cd3c959175daeb9137cc7661c61f064fe31b6fc22a4651a2d1465484b9bd691da1233ca172b070081bb68a82277d62fcd9e1c1b391823976e588b87d4611228e61ffa8c4444ed96e19d64a984110d33b78ce8e108b8eb57004e483ce4f9212d451289a78a47fb642a795faab48fb9542399a956bf9ff7e0d6dba0419c6a3d9e0f61d35a674c1ab35935187d282fa6c9bc989c64e495d4d6ea140ce34b8c76658595659230038d52bca2f44a6a40c13865b89430d6b120489a0447a564a78e932f48cb6af0fedc264ed37bad016059da0ccaf2e6473bc856749a0bd3cfdc8c636cb40ac6f519457d0e79203a6acd72f32f9d499772df01d901e0231d522bbf2c9a9067e373c86f9e8d35cfe35463f6b686c6b6a5417d72854710e9ae9ff675d0bb5f4369d42247d53a15ab3bac20be5616185ffaa9b45774579245c38468c49aafe578dd94e6dd4f5b85a0aba739caa316c573fd64ef57f5fce584c6f41fcbc18b86efbc28f940c61567a61a9e1498d80d1dd91dc342138b64a59e96da2079fad89d180d0b0500919dd915ebb341c1a4199f6dcc789481ace5d1536236a5d2111d149ca8f246751f7f2121c2f71c99e954ec2e09722a4aa9a2ad9198e11a1a883dbda0b7b83cea73e0799b6068373a0431ccd2410b60a836a9f03e219499855b0b88daeb0b79b340dc8ff9168f92e82d0ea3489b78e07a9deb758d5ee7fc3fbe4295de8e2c7f2fa110cac7bdf957250d85b8c3f7f588ec7959a1a3e370a8d0eb93ee7e1e20419e8ef9855844da290352351003ec7bef34834a2e265091a8819b9a5b6fe2339b1e5fc2e131b43facee1fdfb8d6dec3261ed6a4465a3a288e84d26ff2c4e6fe936758b60fce7c11442da64a9c0afce7ce4cb903ac86c1555b455fcffe307e95ae068433029505d859e9beac4f4266aebb77891b73bf6e29f80012e07c2d2aeed75a9ad7d5ac684f1d0c31491ca0618b0576190a01dfce02ee3b71f5a7ba532b308b9b81ec18384fdae3bea76dee7d3352d055027c6293e7e452bea79a0a3f7e299a9fcaba1df0bbeab5c3198cb8b177d98396963e8c8e060d139f493f8dafa1fd39007bee5062e0263c9e626104abe704b46ded819924f16978585be471905821f4f32e0865d03c96ff92f8538fd8ed7d49e28109c5b49d26e1d6710a167f25f135a65156a8b857a782a4764576c12abb614e5207a3cf4f782eb62b25a6dc46a22680d56c57d8b945a9c37fff2df16d3433919886a6005db1a355515d14f6163b91c73acfb8f0343b20b3b19675edc2aad8ef47374479061df896c5660b6c571302b003c7f48b4eab0eeef76cc168168b5bfa0020c9b9c83958046b02a9a428cd236d0a39f9746d756f5f6281dfe05eb9463d3bd2f821778cb8206bb59acd713a33204"

  val encode = BitcoinSUtil.encodeHex(_: Seq[Byte])

  "RawZcashTransactionParser" must "parse a raw v1 transaction" in {
    val tx: ZcashTransaction = RawZcashTransactionParser.read(rawTxV1)
    tx.overwintered must be(false)
    tx.version must be(UInt32.one)
    tx.inputs.size must be(1)
    tx.outputs.size must be(2)
    tx.lockTime must be(UInt32.zero)
    tx.joinSplits.size must be(0)
    tx.txId.hex must be(BitcoinSUtil.flipEndianness("4070553dbcd801788e381692c8cf78cea3ed690ec93b359c5d91848dcc4e3a6c"))
  }

  it must "parse a raw v2 transaction" in {
    val tx: ZcashTransaction = RawZcashTransactionParser.read(rawTxV2)
    tx.overwintered must be(false)
    tx.version must be(UInt32(2))
    tx.inputs.size must be(1)
    tx.outputs.size must be(0)
    tx.lockTime must be(UInt32.zero)
    tx.joinSplits.size must be(1)
    tx.joinSplits.head.vpubOld must be(Satoshis(Int64(1001771776)))
    tx.joinSplits.head.vpubNew must be(Satoshis(Int64(0)))
    tx.txId.hex must be(BitcoinSUtil.flipEndianness("bf1706af01f60f5347eac2291358def1467cae709223018c0bfff38732390c67"))
  }

  it must "parse a raw v3 transaction" in {
    val tx: ZcashTransaction = RawZcashTransactionParser.read(rawTxV3)
    tx.overwintered must be(true)
    tx.version must be(UInt32(3))
    tx.inputs.size must be(5)
    tx.outputs.size must be(0)
    tx.lockTime must be(UInt32.zero)
    tx.expiryHeight must be(UInt32(126))
    tx.joinSplits.size must be(1)
    tx.joinSplits.head.vpubOld must be(Bitcoins(49.9999).satoshis)
    tx.joinSplits.head.vpubNew must be(Satoshis(Int64(0)))
    tx.txId.hex must be(BitcoinSUtil.flipEndianness("ec6711f8250e1f7972789c937220fc1171e9e93052fc6f0e8e4955f6b5a47530"))
  }

  it must "read and write a raw v1 tx" in {
    val tx = RawZcashTransactionParser.read(rawTxV1)
    val serializedTx = RawZcashTransactionParser.write(tx)
    encode(serializedTx) must be(rawTxV1)
  }

  it must "read and write a raw v2 tx" in {
    val tx = RawZcashTransactionParser.read(rawTxV2)
    val serializedTx = RawZcashTransactionParser.write(tx)
    encode(serializedTx) must be(rawTxV2)
  }

  it must "read and write a raw v3 tx" in {
    val tx = RawZcashTransactionParser.read(rawTxV3)
    val serializedTx = RawZcashTransactionParser.write(tx)
    encode(serializedTx) must be(rawTxV3)
  }
}
