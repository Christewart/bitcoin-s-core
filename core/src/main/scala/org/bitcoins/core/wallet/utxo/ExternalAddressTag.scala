package org.bitcoins.core.wallet.utxo

/**
  * Address Tag Names defined outside the library, used for other projects
  * creating there own address tags that aren't supported by bitcoin-s
  */
trait ExternalAddressTagName extends AddressTagName

/**
  * Address Tag Types defined outside the library, used for other projects
  * creating there own address tags that aren't supported by bitcoin-s
  */
trait ExternalAddressTagType extends AddressTagType {
  require(InternalAddressTagType.fromStringOpt(typeName).isEmpty,
          "Cannot extend InternalAddressTagType externally")
}

/**
  * Address Tags defined outside the library, used for other projects
  * creating there own address tags that aren't supported by bitcoin-s
  */
trait ExternalAddressTag extends AddressTag {
  override def tagType: ExternalAddressTagType

  override def tagName: ExternalAddressTagName
}
