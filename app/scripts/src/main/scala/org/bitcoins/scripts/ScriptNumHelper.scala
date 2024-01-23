package org.bitcoins.scripts

import org.bitcoins.core.script.constant.ScriptConstant
import org.bitcoins.crypto.DoubleSha256DigestBE

import java.nio.file.{Files, Paths}

case class ScriptNumHelper(
    txIdBE: DoubleSha256DigestBE,
    scriptConstants: Vector[ScriptConstant],
    sizeIncrease: Long,
    comment: String)

object ScriptNumHelper {

  import org.bitcoins.commons.serializers.Picklers.{
    doubleSha256DigestBEPickler,
    scriptConstantPickler
  }

  implicit val scriptNumHelperRw: upickle.default.ReadWriter[
    ScriptNumHelper] = {
    upickle.default.macroRW[ScriptNumHelper]
  }

  def sizeIncrease(scriptConstants: Vector[ScriptConstant]): Long = {
    val f: Vector[ScriptConstant] = scriptConstants.filter(_.bytes.size < 8)
    f.foldLeft(0L) { case (acc: Long, sc: ScriptConstant) =>
      acc + (8 - sc.byteSize)
    }
  }

  val fileName = "scriptnumcount.json"
  val path = Paths.get(fileName)
  val inputStream = Files.newInputStream(path)
}
