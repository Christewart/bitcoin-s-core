package org.bitcoins.core.wallet.utxo

import org.bitcoins.testkit.util.BitcoinSUnitTest

class AddressTagTest extends BitcoinSUnitTest {

  behavior of "AddressTag"

  it must "read StorageLocationTag from string" in {
    StorageLocationTag.fromString("HotStorage") must be(
      StorageLocationTag.HotStorage)

    StorageLocationTag.fromString("ColdStorage") must be(
      StorageLocationTag.ColdStorage)

    StorageLocationTag.fromString("DeepColdStorage") must be(
      StorageLocationTag.DeepColdStorage)
  }

  it must "read StorageLocationTagName from string" in {
    InternalAddressTagName.fromString("HotStorage") must be(
      StorageLocationTag.HotStorageName)

    InternalAddressTagName.fromString("ColdStorage") must be(
      StorageLocationTag.ColdStorageName)

    InternalAddressTagName.fromString("DeepColdStorage") must be(
      StorageLocationTag.DeepColdStorageName)
  }

  it must "not be able to wrap an internal tag with ExternalAddressTagWrapper" in { _=>
    //why even allow this to run when you can just get a compiler error?
    assertThrows[IllegalArgumentException] {
      ExternalAddressTagWrapper(StorageLocationTag.ColdStorage.tagName,StorageLocationTag.tagType)
    }
  }
}
