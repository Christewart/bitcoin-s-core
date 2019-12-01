package org.bitcoins.keymanager

import java.nio.file.{Files, Path}

import org.bitcoins.core.crypto.{AesPassword, MnemonicCode}
import org.bitcoins.keymanager.ReadMnemonicError.{DecryptionError, JsonParsingError}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.core.gen.CryptoGenerators
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.{BeforeAndAfterEach, FutureOutcome}

class WalletStorageTest
    extends BitcoinSWalletTest
    with BeforeAndAfterEach {

  override type FixtureParam = WalletAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withWalletConfig(test)

  def getSeedPath(config: WalletAppConfig): Path = {
    config.datadir.resolve(WalletAppConfig.ENCRYPTED_SEED_FILE_NAME)
  }

  behavior of "WalletStorage"

  val passphrase = AesPassword.fromNonEmptyString("this_is_secret")
  val badPassphrase = AesPassword.fromNonEmptyString("this_is_also_secret")

  def getAndWriteMnemonic(walletConf: WalletAppConfig): MnemonicCode = {
    val mnemonic = CryptoGenerators.mnemonicCode.sampleSome
    val encrypted = EncryptedMnemonicHelper.encrypt(mnemonic, passphrase)
    val seedPath = getSeedPath(walletConf)
    val _ =
      WalletStorage.writeMnemonicToDisk(seedPath, encrypted)
    mnemonic
  }

  it must "write and read a mnemonic to disk" in { walletConf: WalletAppConfig =>
    assert(!walletConf.seedExists())

    val writtenMnemonic = getAndWriteMnemonic(walletConf)

    // should have been written by now
    assert(walletConf.seedExists())
    val seedPath = getSeedPath(walletConf)
    val read =
      WalletStorage.decryptMnemonicFromDisk(seedPath,passphrase)
    read match {
      case ReadMnemonicSuccess(readMnemonic) =>
        assert(writtenMnemonic == readMnemonic)
      case err: ReadMnemonicError => fail(err.toString)
    }
  }

  it must "fail to read a mnemonic with bad password" in { walletConf =>
    val writtenMnemonic = getAndWriteMnemonic(walletConf)
    val seedPath = getSeedPath(walletConf)
    val read = WalletStorage.decryptMnemonicFromDisk(seedPath, badPassphrase)

    read match {
      case ReadMnemonicSuccess(mnemonic) =>
        fail("Wrote and read with different passwords")
      case DecryptionError        => succeed
      case err: ReadMnemonicError => fail(err.toString)
    }
  }

  it must "fail to read a mnemonic that has bad JSON in it" in { walletConf =>
    val badJson =
      """
        | {
        |   "iv":"ba7722683dad8067df8d069ee04530cc",
        |   "cipherText":,
        |   "salt":"2b7e7d718139518070a87fbbda03ea33cdcda83b555020e9344774e6e7d08af2"
        | }
    """.stripMargin
    val seedPath = getSeedPath(walletConf)
    Files.write(seedPath,
                badJson.getBytes())

    val read =
      WalletStorage.decryptMnemonicFromDisk(seedPath,passphrase)

    read match {
      case JsonParsingError(_)     => succeed
      case res: ReadMnemonicResult => fail(res.toString())
    }
  }

  it must "throw an exception if we attempt to overwrrite an existing seed" in { walletConf =>

    assert(!walletConf.seedExists())

    val _ = getAndWriteMnemonic(walletConf)

    // should have been written by now
    assert(walletConf.seedExists())

    val exn: RuntimeException = intercept[RuntimeException] {
      //attempt to write another mnemonic
      getAndWriteMnemonic(walletConf)
    }

    //need this since we are using async testing suite
    //and exn is not a Future
    succeed

  }
}
