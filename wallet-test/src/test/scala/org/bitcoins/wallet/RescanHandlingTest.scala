package org.bitcoins.wallet

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.wallet.{
  BitcoinSWalletTestCachedBitcoinV19,
  WalletWithBitcoind,
  WalletWithBitcoindV19
}

import scala.concurrent.Future

class RescanHandlingTest extends BitcoinSWalletTestCachedBitcoinV19 {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = WalletWithBitcoind

  behavior of "Wallet rescans"

  it must "properly clear utxos and address for an account" in {
    fixture: WalletWithBitcoind =>
      val wallet = fixture.wallet

      for {
        accountDb <- wallet.getDefaultAccount()
        account = accountDb.hdAccount
        utxos <- wallet.spendingInfoDAO.findAllForAccount(account)
        _ = assert(utxos.nonEmpty)

        addresses <- wallet.addressDAO.findAllForAccount(account)
        _ = assert(addresses.nonEmpty)

        _ <- wallet.clearUtxosAndAddresses(account)

        clearedUtxos <- wallet.spendingInfoDAO.findAllForAccount(account)
        clearedAddresses <- wallet.addressDAO.findAllForAccount(account)
      } yield {
        assert(clearedUtxos.isEmpty)
        assert(clearedAddresses.isEmpty)
      }
  }

  it must "properly clear all utxos and address" in {
    fixture: WalletWithBitcoind =>
      val wallet = fixture.wallet

      for {
        balance <- wallet.getBalance()
        _ = assert(balance != Satoshis.zero)
        utxos <- wallet.spendingInfoDAO.findAll()
        _ = assert(utxos.nonEmpty)

        addresses <- wallet.addressDAO.findAll()
        _ = assert(addresses.nonEmpty)

        _ <- wallet.clearAllUtxosAndAddresses()

        clearedUtxos <- wallet.spendingInfoDAO.findAll()
        clearedAddresses <- wallet.addressDAO.findAll()
      } yield {
        assert(clearedUtxos.isEmpty)
        assert(clearedAddresses.isEmpty)
      }
  }

  val DEFAULT_ADDR_BATCH_SIZE = 1000
  it must "be able to discover funds that belong to the wallet using WalletApi.rescanNeutrinoWallet" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, _) = fixture

      val initBalanceF = wallet.getBalance()

      val rescanF = for {
        initBalance <- initBalanceF
        _ =
          assert(initBalance > CurrencyUnits.zero,
                 s"Cannot run rescan test if our init wallet balance is zero!")
        _ <- wallet.fullRescanNeutrinoWallet(DEFAULT_ADDR_BATCH_SIZE)
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterRescan == initBalance)
      }

      rescanF
  }

  it must "be able to discover funds that occurred within a certain range" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture

      val amt = Bitcoins.one
      val numBlocks = 1

      //send funds to a fresh wallet address
      val addrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val initBlockHeightF = wallet.chainQueryApi.getBestHashBlockHeight()
      val newTxWalletF = for {
        addr <- addrF
        txid <- bitcoind.sendToAddress(addr, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= amt)
        assert(amt == unconfirmedBalance)
        newTxWallet
      }

      //let's clear the wallet and then do a rescan for the last numBlocks
      //that means the wallet should only contain the amt we just processed
      for {
        newTxWallet <- newTxWalletF
        initBlockHeight <- initBlockHeightF
        txInBlockHeight = initBlockHeight + numBlocks
        txInBlockHeightOpt = Some(BlockStamp.BlockHeight(txInBlockHeight))
        _ <- newTxWallet.clearAllUtxosAndAddresses()
        zeroBalance <- newTxWallet.getBalance()
        _ = assert(zeroBalance == Satoshis.zero)
        _ <- newTxWallet.rescanNeutrinoWallet(startOpt = txInBlockHeightOpt,
                                              endOpt = None,
                                              addressBatchSize =
                                                DEFAULT_ADDR_BATCH_SIZE,
                                              useCreationTime = false)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        assert(balance == amt)
        assert(unconfirmedBalance == Bitcoins(1))
      }
  }

  it must "be able to discover funds using multiple batches" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture

      val amt = Bitcoins.one
      val numBlocks = 1

      //send funds to a fresh wallet address
      val addrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val newTxWalletF = for {
        addr <- addrF
        txid <- bitcoind.sendToAddress(addr, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= amt)
        assert(amt == unconfirmedBalance)
        newTxWallet
      }

      for {
        newTxWallet <- newTxWalletF

        account <- newTxWallet.getDefaultAccount()
        txIds <-
          newTxWallet.spendingInfoDAO
            .findAllForAccount(account.hdAccount)
            .map(_.map(_.txid))
        blocks <- newTxWallet.transactionDAO
          .findByTxIdBEs(txIds)
          .map(_.flatMap(_.blockHashOpt))

        _ <- newTxWallet.clearAllUtxosAndAddresses()
        scriptPubKeys <-
          1.to(10).foldLeft(Future.successful(Vector.empty[ScriptPubKey])) {
            (prevFuture, _) =>
              for {
                prev <- prevFuture
                address <- newTxWallet.getNewAddress(account)
                changeAddress <- newTxWallet.getNewChangeAddress(account)
              } yield prev :+ address.scriptPubKey :+ changeAddress.scriptPubKey
          }
        matches <- newTxWallet.getMatchingBlocks(scriptPubKeys,
                                                 None,
                                                 None,
                                                 batchSize = 1)
      } yield {
        assert(matches.size == blocks.size)
        assert(
          matches.forall(blockMatch => blocks.contains(blockMatch.blockHash)))
      }
  }

  it must "be able to discover funds that occurred from the wallet creation time" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture

      val amt = Bitcoins.one
      val numBlocks = 1

      //send funds to a fresh wallet address
      val addrF = wallet.getNewAddress()
      val bitcoindAddrF = bitcoind.getNewAddress
      val newTxWalletF = for {
        addr <- addrF
        txid <- bitcoind.sendToAddress(addr, amt)
        tx <- bitcoind.getRawTransactionRaw(txid)
        bitcoindAddr <- bitcoindAddrF
        blockHashes <-
          bitcoind.generateToAddress(blocks = numBlocks, address = bitcoindAddr)
        newTxWallet <- wallet.processTransaction(transaction = tx,
                                                 blockHashOpt =
                                                   blockHashes.headOption)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        //balance doesn't have to exactly equal, as there was money in the
        //wallet before hand.
        assert(balance >= amt)
        assert(amt == unconfirmedBalance)
        newTxWallet
      }

      for {
        newTxWallet <- newTxWalletF
        _ <- newTxWallet.rescanNeutrinoWallet(startOpt = None,
                                              endOpt = None,
                                              addressBatchSize =
                                                DEFAULT_ADDR_BATCH_SIZE,
                                              useCreationTime = true)
        balance <- newTxWallet.getBalance()
        unconfirmedBalance <- newTxWallet.getUnconfirmedBalance()
      } yield {
        assert(balance == Bitcoins(7))
        assert(unconfirmedBalance == Bitcoins(1))
      }
  }

  it must "NOT discover funds that happened OUTSIDE of a certain range of block hashes" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, _) = fixture

      val initBalanceF = wallet.getBalance()

      //find the first block a utxo was created in
      val utxosF = wallet.listUtxos()
      val oldestHeightF = for {
        utxos <- utxosF
        blockhashes <- wallet.transactionDAO
          .findByTxIdBEs(utxos.map(_.txid))
          .map(_.flatMap(_.blockHashOpt))
        heights <- FutureUtil.sequentially(blockhashes) { hash =>
          wallet.chainQueryApi.getBlockHeight(hash)
        }
      } yield heights.min.get

      //ok now that we have the height of the oldest utxo, let's rescan up to then
      val rescanF = for {
        initBalance <- initBalanceF
        _ =
          assert(initBalance > CurrencyUnits.zero,
                 s"Cannot run rescan test if our init wallet balance is zero!")
        oldestUtxoHeight <- oldestHeightF
        end = Some(BlockStamp.BlockHeight(oldestUtxoHeight - 1))
        _ <- wallet.rescanNeutrinoWallet(startOpt = BlockStamp.height0Opt,
                                         endOpt = end,
                                         addressBatchSize =
                                           DEFAULT_ADDR_BATCH_SIZE,
                                         useCreationTime = false)
        balanceAfterRescan <- wallet.getBalance()
      } yield {
        assert(balanceAfterRescan == CurrencyUnits.zero)
      }

      rescanF
  }

  it must "still receive payments to addresses generated pre-rescan" in {
    fixture: WalletWithBitcoind =>
      val WalletWithBitcoindV19(wallet, bitcoind) = fixture
      logger.info(s"Beginning test case")
      val addressNoFundsF = wallet.getNewAddress()

      //start a rescan without sending payment to that address
      for {
        address <- addressNoFundsF
        _ = logger.info(s"Beginning test case address=$address")
        _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                         endOpt = None,
                                         addressBatchSize = 10,
                                         useCreationTime = false)
        _ <- AsyncUtil.retryUntilSatisfiedF(() => {
          wallet.isRescanning().map(isRescanning => !isRescanning)
        })

        unusedAddresses <- wallet.listUnusedAddresses()
        usedAddresses <- wallet.listFundedAddresses()

        spks <- wallet.listUtxos().map(_.map(_.output.scriptPubKey))
        _ = spks.foreach(spk =>
          logger.info(
            s"spk=$spk addressSpk=${address.scriptPubKey} isMatch=${spk == address.scriptPubKey}"))
        _ = assert(!usedAddresses.exists(_._1.address == address),
                   s"Address should not be used! address=$address")
        _ = assert(unusedAddresses.exists(_.address == address),
                   s"Address should be UNUSED! address=$address")
        //now send a payment to our wallet
        hashes <- bitcoind.generateToAddress(1, address)
        block <- bitcoind.getBlockRaw(hashes.head)
        _ = logger.info(
          s"address=${address} spk=${address.scriptPubKey} txid=${block.transactions.head.txIdBE.hex}")
        _ <- wallet.processBlock(block)
        fundedAddresses <- wallet.listFundedAddresses()
      } yield {
        val addressExists = fundedAddresses.exists(_._1.address == address)
        assert(addressExists)
      }

  }

}
