package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{
  NeutrinoNodeFundedWalletBitcoind,
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class NeutrinoNodeWithWalletTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeFundedWalletBitcoind

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeFundedWalletBitcoind(
        test = test,
        bip39PasswordOpt = getBIP39PasswordOpt(),
        bitcoind = bitcoind
      )(system, getFreshConfig)
      f <- outcome.toFuture
    } yield f

    new FutureOutcome(outcomeF)
  }

  val TestAmount = 1.bitcoin
  val FeeRate = SatoshisPerByte(10.sats)
  val TestFees: Satoshis = 2230.sats

  it must "receive information about received payments" in { param =>
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

    def condition(
        expectedConfirmedAmount: CurrencyUnit,
        expectedUnconfirmedAmount: CurrencyUnit,
        expectedUtxos: Int,
        expectedAddresses: Int): Future[Boolean] = {
      for {
        confirmedBalance <- wallet.getConfirmedBalance()
        unconfirmedBalance <- wallet.getUnconfirmedBalance()
        addresses <- wallet.listAddresses()
        utxos <- wallet.listDefaultAccountUtxos()
      } yield {
        // +- fee rate because signatures could vary in size
        (expectedConfirmedAmount === confirmedBalance +- FeeRate.currencyUnit) &&
        (expectedUnconfirmedAmount === unconfirmedBalance +- FeeRate.currencyUnit) &&
        (expectedAddresses == addresses.size) &&
        (expectedUtxos == utxos.size)
      }
    }

    //default wallet utxos are 3BTC, 2BTC, 1BTC
    //our coin selection algorithm seems to be selecting
    //the 3BTC utxo to spend, so we should have
    //confirmed = 2BTC + 1BTC
    //unconfirmed = 3 BTC - TestAmount - TestFees
    val condition1 = () => {
      condition(
        expectedConfirmedAmount = 3.bitcoin,
        expectedUnconfirmedAmount = 3.bitcoin - TestAmount - TestFees,
        expectedUtxos = 3,
        expectedAddresses = 7
      )
    }

    //this is just sending TestAmount back to us
    //so everything should stay the same as above
    //expected we should have received TestAmount back
    //and have 1 more address/utxo
    val condition2 = { () =>
      condition(
        expectedConfirmedAmount = 3.bitcoin,
        expectedUnconfirmedAmount =
          (3.bitcoin - TestAmount - TestFees) + TestAmount,
        expectedUtxos = 4,
        expectedAddresses = 8
      )
    }

    for {
      // send
      addr <- bitcoind.getNewAddress
      _ <- wallet.sendToAddress(addr, TestAmount, Some(FeeRate))

      _ <- wallet.getConfirmedBalance()
      _ <- wallet.getUnconfirmedBalance()
      _ <- wallet.getBalance()
      _ <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
      _ <- wallet.getConfirmedBalance()
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      _ <- AsyncUtil.awaitConditionF(condition1)
      // receive
      address <- wallet.getNewAddress()
      txId <- bitcoind.sendToAddress(address, TestAmount)
      expectedTx <- bitcoind.getRawTransactionRaw(txId)

      _ <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFilterHeadersSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      _ <- AsyncUtil.awaitConditionF(condition2)
      // assert we got the full tx with witness data
      txs <- wallet.listTransactions()
    } yield assert(txs.exists(_.transaction == expectedTx))
  }

  it must "watch an arbitrary SPK" in { param =>
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

    def generateBlock() =
      for {
        _ <-
          bitcoind.getNewAddress
            .flatMap(bitcoind.generateToAddress(1, _))
        _ <- NodeTestUtil.awaitSync(node, bitcoind)
        _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      } yield ()

    val pk1 = ECPublicKey.freshPublicKey
    val pk2 = ECPublicKey.freshPublicKey
    val spk = MultiSignatureScriptPubKey(2, Vector(pk1, pk2))
    val sats = TestAmount
    val output = TransactionOutput(sats, spk)

    for {
      // start watching
      _ <- wallet.watchScriptPubKey(spk)

      // send
      txSent <- wallet.sendToOutputs(Vector(output), FeeRate)
      _ <- node.broadcastTransaction(txSent)

      // confirm
      _ <- generateBlock()
      _ <- generateBlock()
      _ <- generateBlock()

      // verify
      txs <- wallet.listTransactions()
    } yield assert(txs.exists(_.txIdBE == txSent.txIdBE))
  }

  it must "rescan information about received payments" in { param =>
    val NeutrinoNodeFundedWalletBitcoind(node, wallet, bitcoind, _) = param

    def condition(): Future[Boolean] = {
      for {
        balance <- wallet.getBalance()
        addresses <- wallet.listAddresses()
        utxos <- wallet.listUtxos()
      } yield {
        val expectedBalance = BitcoinSWalletTest.expectedDefaultAmt + TestAmount
        val expectedUtxoScripts = utxos
          .map(_.output.scriptPubKey.hex)
          .sorted
        val expectedAddressScripts = addresses.map(_.scriptPubKey.hex).sorted
        logger.info(
          s"expectedBalance=${expectedBalance} balance=${balance} utxos.size=${utxos.size}")
        logger.info(
          s"expectedAddressScripts.length=${expectedAddressScripts.length} expectedUtxoScripts.length=${expectedUtxoScripts.length}")
        balance == expectedBalance &&
        utxos.size == 4 &&
        expectedAddressScripts == expectedUtxoScripts
      }
    }

    for {
      addresses <- wallet.listAddresses()
      _ = logger.warn(s"0")
      utxos <- wallet.listDefaultAccountUtxos()
      _ = assert(addresses.size == 6)
      _ = assert(utxos.size == 3)

      address <- wallet.getNewAddress()
      _ <-
        bitcoind
          .sendToAddress(address, TestAmount)

      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      _ = assert(addresses.size == 7)
      _ = assert(utxos.size == 3)
      _ = logger.warn(s"2")
      _ <- wallet.clearAllUtxosAndAddresses()
      _ = logger.warn(s"3")
      addresses <- wallet.listAddresses()
      utxos <- wallet.listDefaultAccountUtxos()
      _ = assert(addresses.isEmpty)
      _ = assert(utxos.isEmpty)

      _ <-
        bitcoind.getNewAddress
          .flatMap(bitcoind.generateToAddress(1, _))
      _ = logger.warn(s"4")
      _ <- NodeTestUtil.awaitSync(node, bitcoind)
      _ <- NodeTestUtil.awaitCompactFiltersSync(node, bitcoind)
      _ = logger.warn(s"5")
      _ <- wallet.fullRescanNeutrinoWallet(addressBatchSize = 7)

      _ <- AsyncUtil.awaitConditionF(condition)
    } yield {
      logger.warn(s"6")
      succeed
    }
  }
}
