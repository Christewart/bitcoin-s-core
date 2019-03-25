package org.bitcoins.wallet.util

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.UnitTestDbConfig
import org.bitcoins.wallet.config.WalletDbManagement
import org.bitcoins.wallet.models.{AccountDAO, AddressDAO, MnemonicCodeDAO}
import org.scalatest.{AsyncFlatSpec, BeforeAndAfterAll}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext}

trait BitcoinSWalletTest
    extends AsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected lazy val dbConfig: UnitTestDbConfig.type = UnitTestDbConfig
  protected val chainParams: ChainParams = WalletTestUtil.chainParams

  protected val addressDAO: AddressDAO = AddressDAO(dbConfig, chainParams)
  protected val accountDAO: AccountDAO = AccountDAO(dbConfig, chainParams)
  protected val mnemonicDAO: MnemonicCodeDAO = MnemonicCodeDAO(dbConfig)

  /** Timeout for async operations */
  protected val timeout: FiniteDuration = 10.seconds

  protected val networkParam: RegTest.type = WalletTestUtil.networkParam

  override protected def beforeAll(): Unit = {
    Await.result(WalletDbManagement.dropAll(dbConfig), timeout)
    Await.result(WalletDbManagement.createAll(dbConfig), timeout)
  }

  override protected def afterAll(): Unit = {
    // Await.result(WalletDbManagement.dropAll(dbConfig), timeout)
  }
}
