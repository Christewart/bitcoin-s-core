package org.bitcoins.testkit.wallet

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.{CachedBitcoind, CachedBitcoindV19}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{
  createWalletWithBitcoind,
  createWalletWithBitcoindCallbacks,
  destroyWalletWithBitcoind,
  fundWalletWithBitcoind
}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** Bitcoin-s wallet test trait that uses cached bitcoinds
  * rather than fresh bitcoinds.
  *
  * This should be used by default unless there is a reason your
  * test suite needs fresh bitcoin's for every unit test inside of it
  */
trait BitcoinSWalletTestCachedBitcoind
    extends BitcoinSFixture
    with BaseWalletTest
    with EmbeddedPg { _: CachedBitcoind =>

  /** Creates a funded wallet fixture with bitcoind
    * This is different than [[withFundedWalletAndBitcoind()]]
    * in the sense that it does NOT destroy the given bitcoind.
    * It is the responsibility of the caller of this method to
    * do that, if needed.
    */
  def withFundedWalletAndBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient,
      bip39PasswordOpt: Option[String]): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = { () =>
      for {
        walletWithBitcoind <- createWalletWithBitcoindCallbacks(
          bitcoind = bitcoind,
          bip39PasswordOpt = bip39PasswordOpt)
        fundedWallet <- fundWalletWithBitcoind(walletWithBitcoind)
        _ <- SyncUtil.syncWalletFullBlocks(wallet = fundedWallet.wallet,
                                           bitcoind = bitcoind)
        _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
      } yield fundedWallet
    }

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withNewWalletAndBitcoindCached(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindRpcClient): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = { () =>
        Future.successful(bitcoind)
      },
      dependentBuilder = { (bitcoind: BitcoindRpcClient) =>
        createWalletWithBitcoind(bitcoind, bip39PasswordOpt)
      },
      wrap = (_: BitcoindRpcClient, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCached(test,
                                                     bitcoind,
                                                     getBIP39PasswordOpt())
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}

trait BitcoinSWalletTestCachedBitcoinV19
    extends BitcoinSWalletTestCachedBitcoind
    with CachedBitcoindV19 {

  /** Creates a funded wallet fixture with bitcoind
    * This is different than [[withFundedWalletAndBitcoind()]]
    * in the sense that it does NOT destroy the given bitcoind.
    * It is the responsibility of the caller of this method to
    * do that, if needed.
    */
  def withFundedWalletAndBitcoindCachedV19(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV19RpcClient,
      bip39PasswordOpt: Option[String]): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoindV19] = { () =>
      for {
        walletWithBitcoind <- createWalletWithBitcoindCallbacks(
          bitcoind = bitcoind,
          bip39PasswordOpt = bip39PasswordOpt)
        walletWithBitcoindV19 = WalletWithBitcoindV19(walletWithBitcoind.wallet,
                                                      bitcoind)
        fundedWallet <- fundWalletWithBitcoind[WalletWithBitcoindV19](
          walletWithBitcoindV19)
        _ <- SyncUtil.syncWalletFullBlocks(wallet = fundedWallet.wallet,
                                           bitcoind = bitcoind)
        _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
      } yield fundedWallet
    }

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withNewWalletAndBitcoindCachedV19(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindV19RpcClient): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = { () =>
        Future.successful(bitcoind)
      },
      dependentBuilder = { (bitcoind: BitcoindV19RpcClient) =>
        BitcoinSWalletTest.createWalletWithBitcoindV19(bitcoind,
                                                       bip39PasswordOpt)
      },
      wrap =
        (_: BitcoindRpcClient, walletWithBitcoind: WalletWithBitcoindV19) =>
          walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCachedV19(test,
                                                        bitcoind,
                                                        getBIP39PasswordOpt())
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}
