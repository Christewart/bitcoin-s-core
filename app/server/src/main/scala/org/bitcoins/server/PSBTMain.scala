package org.bitcoins.server

import org.bitcoins.core.hd.{HDPath, SegWitHDPath}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.crypto.Sign
import org.bitcoins.keymanager.bip39.BIP39KeyManager

import scala.concurrent.Future

object PSBTMain extends App with BitcoinSRunner {

  implicit val actorSystemName = "hello"

  override def startup: Future[Unit] = {
    val dorierPSBT: String = {
      s"cHNidP8BAHECAAAAAaPTWHbUU9qZH3E8kIkbibcteaNNTH6csUa+pkkyrqdQAAAAAAD+////AgCHkwMAAAAAFgAU0vJ6xEpYLZgq0XT4Lxnrz3UcBhsAWmICAAAAABYAFMwT65eVs7MNdEmZ8a/btlp3iIuzAAAAAAABASua5/UFAAAAACIAIPrv3JDIVLeft5JpaP948L/cyobLz9N9fzvM5qNeV34tAQVHUiEDgsVFqH1kh8VRENc3ORR0Nyf7G4DuBuTJxi4ln6dRMTMhAnuk3VLqOOGKWaQyBUAiA0Dkh9fJlvN+voAZ2YE4q6lGUq4iAgJ7pN1S6jjhilmkMgVAIgNA5IfXyZbzfr6AGdmBOKupRkcwRAIgYCTrwRVMg9cxXtXAlIu0Ppmm6pwzxsICTFVM0zPvbKoCIGB2gGSoD6BM8wxHN0WWBtUsbG+J93jPThMKWkGlonkwAQAAAA=="
    }

    val main = new BitcoinSServerMain(Array.empty)
    implicit val walletConf = main.conf.walletConf
    val dorierSignedPsbt = PSBT.fromBase64(dorierPSBT)
    val kmParams = walletConf.kmParams

    val keyManagerE =
      BIP39KeyManager.fromParams(kmParams = kmParams,
                                 password = BIP39KeyManager.badPassphrase,
                                 bip39PasswordOpt = None)

    val keyManager =
      keyManagerE.getOrElse(sys.error(s"Could not get key manager"))
    println(s"keyManager=${keyManager}")
    //val spendingInfoDAO = SpendingInfoDAO()

    val hdPathF: Future[HDPath] = {
      val h = SegWitHDPath.fromStringT(s"m/84'/0'/0'/0/6")
      Future.fromTry(h)
    }

    hdPathF.failed.foreach(err => logger.error(s"hdPath error", err))
    val signF: Future[Sign] = for {
      hdPath <- hdPathF
      _ = println(s"hdPath=$hdPath")
    } yield {
      keyManager.toSign(hdPath)
    }

    val signedPsbtF = {
      for {
        sign <- signF
        _ = logger.error(s"Attempting to sign PSBT")
        signedPSBT <- dorierSignedPsbt.sign(0, sign)
      } yield signedPSBT
    }

    signedPsbtF.failed.foreach(err => logger.error(s"Failed to sign PSBT", err))

    val finalizedPSBTF = signedPsbtF.flatMap { signedPsbt =>
      val finalizeT = signedPsbt.finalizePSBT
      println(s"finalizeT=${finalizeT}")
      Future.fromTry(finalizeT)
    }

    for {
      finalizedPSBT <- finalizedPSBTF
    } yield {
      println(s"Final tx=${finalizedPSBT.extractTransaction}")
    }
  }

  startup

}
