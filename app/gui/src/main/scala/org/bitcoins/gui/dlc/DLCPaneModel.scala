package org.bitcoins.gui.dlc

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.{CliCommand, Config, ConsoleCli}
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.SingleNonceOracleInfo
import org.bitcoins.commons.jsonmodels.dlc.DLCStatus
import org.bitcoins.crypto.{ECPrivateKey, Sha256DigestBE}
import org.bitcoins.gui.dlc.dialog._
import org.bitcoins.gui.{GlobalData, TaskRunner}
import scalafx.beans.property.ObjectProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{TextArea, TextField}
import scalafx.stage.Window

import scala.util.{Failure, Success}

class DLCPaneModel(
    resultArea: TextArea,
    oracleInfoArea: TextArea,
    numOutcomesTF: TextField) {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  val dlcs: ObservableBuffer[DLCStatus] = new ObservableBuffer[DLCStatus]()

  def getDLCs: Vector[DLCStatus] = {
    ConsoleCli.exec(GetDLCs, Config.empty) match {
      case Failure(exception) => throw exception
      case Success(dlcsStr) =>
        ujson.read(dlcsStr).arr.map(DLCStatus.fromJson).toVector
    }
  }

  def setUp(): Unit = {
    dlcs.clear()
    dlcs ++= getDLCs
  }

  def updateDLC(paramHash: Sha256DigestBE): Unit = {
    ConsoleCli.exec(GetDLC(paramHash), Config.empty) match {
      case Failure(exception) => throw exception
      case Success(dlcStatus) =>
        dlcs += DLCStatus.fromJson(ujson.read(dlcStatus))
        dlcs.find(_.paramHash == paramHash).foreach(dlcs -= _)
    }
  }

  def updateDLCs(): Unit = {
    val newDLCs = getDLCs
    val toAdd = newDLCs.diff(dlcs)
    val toRemove = dlcs.diff(newDLCs)
    dlcs ++= toAdd
    dlcs --= toRemove
  }

  def printDLCDialogResult[T <: CliCommand](
      caption: String,
      dialog: DLCDialog[T]): Unit = {
    val result = dialog.showAndWait(parentWindow.value)

    result match {
      case Some(command) =>
        taskRunner.run(
          caption = caption,
          op = {
            ConsoleCli.exec(command, GlobalData.consoleCliConfig) match {
              case Success(commandReturn) => resultArea.text = commandReturn
              case Failure(err) =>
                err.printStackTrace()
                resultArea.text = s"Error executing command:\n${err.getMessage}"
            }
            updateDLCs()
          }
        )
      case None => ()
    }
  }

  def onInitOracle(): Unit = {
    val numOutcomes = BigInt(numOutcomesTF.text()).toInt
    require(numOutcomes <= 10, "More than 10 outcomes not supported.")

    val result = InitOracleDialog.showAndWait(parentWindow.value, numOutcomes)

    result match {
      case Some((outcomes, contractInfo)) =>
        val builder = new StringBuilder()

        val privKey = ECPrivateKey.freshPrivateKey
        val pubKey = privKey.schnorrPublicKey
        val kValue = ECPrivateKey.freshPrivateKey
        val rValue = kValue.schnorrNonce
        val oracleInfo = SingleNonceOracleInfo(pubKey, rValue)

        builder.append(
          s"Oracle Public Key: ${pubKey.hex}\nEvent R value: ${rValue.hex}\n")
        builder.append(s"Serialized Oracle Info: ${oracleInfo.hex}\n\n")

        builder.append("Outcome hashes and amounts in order of entry:\n")
        contractInfo.foreach {
          case (str, amt) => builder.append(s"$str - ${amt.toLong}\n")
        }
        builder.append(s"\nSerialized Contract Info:\n${contractInfo.hex}\n\n")

        builder.append("Outcomes and oracle sigs in order of entry:\n")
        outcomes.zip(contractInfo.keys).foreach {
          case (outcome, str) =>
            val hash = str.serialized.head
            val sig = privKey.schnorrSignWithNonce(hash, kValue)
            builder.append(s"$outcome - ${sig.hex}\n")
        }

        GlobalDLCData.lastOracleInfo = oracleInfo.hex
        GlobalDLCData.lastContractInfo = contractInfo.hex

        oracleInfoArea.text = builder.result()
      case None => ()
    }
  }

  def onOffer(): Unit = {
    printDLCDialogResult("CreateDLCOffer", OfferDLCDialog)
  }

  def onAccept(): Unit = {
    printDLCDialogResult("AcceptDLCOffer", AcceptDLCDialog)
  }

  def onSign(): Unit = {
    printDLCDialogResult("SignDLC", SignDLCDialog)
  }

  def onAddSigs(): Unit = {
    printDLCDialogResult("AddDLCSigs", AddSigsDLCDialog)
  }

  def onGetFunding(): Unit = {
    printDLCDialogResult("GetDLCFundingTx", GetFundingDLCDialog)
  }

  def onClose(): Unit = {
    printDLCDialogResult("ExecuteDLC", ForceCloseDLCDialog)
  }

  def onRefund(): Unit = {
    printDLCDialogResult("ExecuteDLCRefund", RefundDLCDialog)
  }

  def viewDLC(status: DLCStatus): Unit = {
    updateDLCs()
    val updatedStatus = dlcs.find(_.tempContractId == status.tempContractId)
    ViewDLCDialog.showAndWait(parentWindow.value,
                              updatedStatus.getOrElse(status))
  }
}
