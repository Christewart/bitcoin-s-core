import org.bitcoins.core.protocol._
import org.bitcoins.core.config.{ MainNet, RegTest, TestNet3 }
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.protocol.ln.{ LNInvoice, LNTags }
import org.scalatest.{ FlatSpec, MustMatchers }

class LnTagsSpec extends FlatSpec with MustMatchers {

  behavior of "LNTags"

  it must "parse BOLT11 example 1" in {
    //BOLT11 Example #1
    val lnTags = LNTags(
      "0001020304050607080900010203040506070809000102030405060708090102",
      Some("Please consider supporting this project"), None, None,
      None, None, None, None)
    LNInvoice(MainNet, None, lnTags,
      ("38ec6891345e204145be8a3a99de38e98a39d6a569434e1845c8af7205afcfcc7f425fcd1463e93c32881ead0d6e356d467ec8c02553f9aab15e5738b11f127f", 0)).Invoice

  }

  it must "parse BOLT11 example 2" in {
    //BOLT11 Example #2
    val lnTags = LNTags(
      "0001020304050607080900010203040506070809000102030405060708090102",
      Some("1 cup coffee"), None, None, Some(60), None, None, None)
    LNInvoice(MainNet, Some(2500, CurrencyUnits.oneMBTC), lnTags,
      ("e89639ba6814e36689d4b91bf125f10351b55da057b00647a8dabaeb8a90c95f160f9d5a6e0f79d1fc2b964238b944e2fa4aa677c6f020d466472ab842bd750e", 1)).Invoice

  }

  it must "parse BOLT11 example 3" in {
    //BOLT11 Example #3 - 1 Issue, description field does not encode correctly due to Japanese letters
    val lnTags = LNTags("0001020304050607080900010203040506070809000102030405060708090102", Some("ナンセンス 1杯"), None,
      None, Some(60), None, None, None)
    LNInvoice(MainNet, Some(2500, CurrencyUnits.oneMBTC), lnTags,
      ("259f04511e7ef2aa77f6ff04d51b4ae9209504843e5ab9672ce32a153681f687515b73ce57ee309db588a10eb8e41b5a2d2bc17144ddf398033faa49ffe95ae6", 0)).Invoice

  }

  it must "parse BOLT11 example 4" in {
    //BOLT11 Example #4
    val lnTags = LNTags("0001020304050607080900010203040506070809000102030405060708090102", None,
      None, Some("3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1"), None, None,
      None, None)
    LNInvoice(MainNet, Some(20, CurrencyUnits.oneBTC), lnTags,
      ("c63486e81f8c878a105bc9d959af1973854c4dc552c4f0e0e0c7389603d6bdc67707bf6be992a8ce7bf50016bb41d8a9b5358652c4960445a170d049ced4558c", 0)).Invoice

  }

  it must "parse BOLT11 example 5" in {
    //BOLT11 Example #5
    val lnTags = LNTags("0001020304050607080900010203040506070809000102030405060708090102", None,
      None, Some("3925b6f67e2c340036ed12093dd44e0368df1b6ea26c53dbe4811f58fd5db8c1"), None, None, Some("71d7daf61a4972a083f98cee48f05ed9090cdd9e"), None)
    LNInvoice(MainNet, Some(20, CurrencyUnits.oneBTC), lnTags,
      ("b6c42b8a61e0dc5823ea63e76ff148ab5f6c86f45f9722af0069c7934daff70d5e315893300774c897995e3a7476c8193693d144a36e2645a0851e6ebafc9d0a", 1)).Invoice
  }

}

