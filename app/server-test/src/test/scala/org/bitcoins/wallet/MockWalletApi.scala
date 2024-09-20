package org.bitcoins.wallet

import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.hd.AddressType

import scala.concurrent.Future

/** ScalaMock cannot stub traits with protected methods, so we need to stub them
  * manually.
  */
abstract class MockWalletApi extends DLCNeutrinoHDWalletApi {

  override def getDefaultAccount(): Future[AccountDb] = stub

  override def getDefaultAccountForType(
      addressType: AddressType
  ): Future[AccountDb] = stub

  private def stub[T] =
    Future.failed[T](new RuntimeException("Not implemented"))

}
