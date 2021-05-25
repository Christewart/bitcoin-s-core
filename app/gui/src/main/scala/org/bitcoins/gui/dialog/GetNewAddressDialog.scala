package org.bitcoins.gui.dialog

import com.google.zxing.BarcodeFormat
import com.google.zxing.common.BitMatrix
import com.google.zxing.qrcode.QRCodeWriter
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.scene.control.{ButtonType, Dialog}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.StackPane
import scalafx.stage.Window

object GetNewAddressDialog {

  def showAndWait(parentWindow: Window, address: String): Unit = {
    showAndWait(parentWindow, StringProperty(address))
  }

  def showAndWait(parentWindow: Window, address: StringProperty): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      title = "New Address"
    }

    // TODO make a button to copy the address to clipboard

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    println(s"address=${address.getValue}")
    val image: Image =
      generateQRCode(address = BitcoinAddress.fromString(address.getValue),
                     200,
                     200)

    val qrView = new ImageView()

    qrView.setImage(image)

    val root = new StackPane()

    root.getChildren().add(qrView)

    //val scene = new Scene(root, 200, 200)

    dialog.graphic = qrView
    val _ = dialog.showAndWait()
  }

  private def generateQRCode(
      address: BitcoinAddress,
      width: Int,
      height: Int): Image = {
    val qrCodeWriter = new QRCodeWriter
    val bitMatrix: BitMatrix =
      qrCodeWriter.encode(address.toString(),
                          BarcodeFormat.QR_CODE,
                          width,
                          height)

    val writableImage = new WritableImage(width, height)
    val pixelWriter = writableImage.getPixelWriter()
    0.until(height).map { x =>
      0.until(width).map { y =>
        val color = if (bitMatrix.get(x, y)) Color.BLACK else Color.WHITE
        pixelWriter.setColor(x, y, color)
      }
    }
    writableImage
  }
}
