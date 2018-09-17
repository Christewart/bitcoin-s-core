package org.bitcoins.core.protocol.ln

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{ BaseNumbers, Int64 }
import org.bitcoins.core.protocol.NetworkElement
import scodec.bits.ByteVector

import scala.math.BigDecimal.RoundingMode
import scala.util.{ Failure, Try }

sealed abstract class LnCurrencyUnit extends NetworkElement {
  type A

  def character: Char

  def multiplier: BigDecimal

  def >=(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue >= ln.toPicoBitcoinValue
  }

  def >(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue > ln.toPicoBitcoinValue
  }

  def <(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue < ln.toPicoBitcoinValue
  }

  def <=(ln: LnCurrencyUnit): Boolean = {
    toPicoBitcoinValue <= ln.toPicoBitcoinValue
  }

  def !=(ln: LnCurrencyUnit): Boolean = !(this == ln)

  def ==(ln: LnCurrencyUnit): Boolean = toPicoBitcoinValue == ln.toPicoBitcoinValue

  def +(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoSatoshis(toPicoBitcoinValue + ln.toPicoBitcoinValue)
  }

  def -(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoSatoshis(toPicoBitcoinValue - ln.toPicoBitcoinValue)
  }

  def *(ln: LnCurrencyUnit): LnCurrencyUnit = {
    PicoSatoshis(toPicoBitcoinValue * ln.toPicoBitcoinValue)
  }

  def unary_- : LnCurrencyUnit = {
    PicoSatoshis(-toPicoBitcoinValue)
  }

  override def bytes: ByteVector = Int64(toPicoBitcoinValue).bytes.reverse

  def toBigInt: BigInt

  def toLong: Long = toBigInt.toLong

  def toInt: Int = {
    require(this.toBigInt >= Int.MinValue, "Number was too small for Int, got: " + underlying)
    require(this.toBigInt <= Int.MaxValue, "Number was too big for Int, got: " + underlying)
    toBigInt.toInt
  }

  protected def underlying: A

  def toSatoshis: Satoshis

  def toPicoBitcoinValue: BigInt

  def toPicoBitcoinMultiplier: Int

  def toEncodedString: String = this.toBigInt + this.character.toString()
}

sealed abstract class MilliSatoshis extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'm'

  override def multiplier: BigDecimal = LnPolicy.milliMultiplier

  override def toPicoBitcoinMultiplier: Int = 1000000000

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = LnCurrencyUnits.toPicoBitcoinValue(this)
}

object MilliSatoshis extends BaseNumbers[MilliSatoshis] {
  val min = MilliSatoshis(LnPolicy.minMilliSatoshis)
  val max = MilliSatoshis(LnPolicy.maxMilliSatoshis)
  val zero = MilliSatoshis(0)
  val one = MilliSatoshis(1)

  def apply(milliSatoshis: Int64): MilliSatoshis = MilliSatoshis(milliSatoshis.toBigInt)

  def apply(underlying: BigInt): MilliSatoshis = MilliSatoshisImpl(underlying)

  private case class MilliSatoshisImpl(underlying: BigInt) extends MilliSatoshis {
    require(underlying >= LnPolicy.minMilliSatoshis, "Number was too small for MilliSatoshis, got: " + underlying)
    require(underlying <= LnPolicy.maxMilliSatoshis, "Number was too big for MilliSatoshis, got: " + underlying)
  }
}

sealed abstract class MicroSatoshis extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'u'

  override def multiplier: BigDecimal = LnPolicy.microMultiplier

  override def toPicoBitcoinMultiplier: Int = 1000000

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = LnCurrencyUnits.toPicoBitcoinValue(this)
}

object MicroSatoshis extends BaseNumbers[MicroSatoshis] {
  val min = MicroSatoshis(LnPolicy.minMicroSatoshis)
  val max = MicroSatoshis(LnPolicy.maxMicroSatoshis)
  val zero = MicroSatoshis(0)
  val one = MicroSatoshis(1)

  def apply(microSatoshis: Int64): MicroSatoshis = MicroSatoshis(microSatoshis.toBigInt)

  def apply(underlying: BigInt): MicroSatoshis = MicroSatoshisImpl(underlying)

  private case class MicroSatoshisImpl(underlying: BigInt) extends MicroSatoshis {
    require(underlying >= LnPolicy.minMicroSatoshis, "Number was too small for MicroSatoshis, got: " + underlying)
    require(underlying <= LnPolicy.maxMicroSatoshis, "Number was too big for MicroSatoshis, got: " + underlying)
  }
}

sealed abstract class NanoSatoshis extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'n'

  override def multiplier: BigDecimal = LnPolicy.nanoMultiplier

  override def toPicoBitcoinMultiplier: Int = 1000

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = LnCurrencyUnits.toPicoBitcoinValue(this)
}

object NanoSatoshis extends BaseNumbers[NanoSatoshis] {
  val min = NanoSatoshis(LnPolicy.minNanoSatoshis)
  val max = NanoSatoshis(LnPolicy.maxNanoSatoshis)
  val zero = NanoSatoshis(0)
  val one = NanoSatoshis(1)

  def apply(nanoSatoshis: Int64): NanoSatoshis = NanoSatoshis(nanoSatoshis.toBigInt)

  def apply(underlying: BigInt): NanoSatoshis = NanoSatoshisImpl(underlying)

  private case class NanoSatoshisImpl(underlying: BigInt) extends NanoSatoshis {
    require(underlying >= LnPolicy.minNanoSatoshis, "Number was too small for NanoSatoshis, got: " + underlying)
    require(underlying <= LnPolicy.maxNanoSatoshis, "Number was too big for NanoSatoshis, got: " + underlying)
  }
}

sealed abstract class PicoSatoshis extends LnCurrencyUnit {
  override type A = BigInt

  override def character: Char = 'p'

  override def multiplier: BigDecimal = LnPolicy.picoMultiplier

  override def toPicoBitcoinMultiplier: Int = 1

  override def toBigInt: A = underlying

  override def toSatoshis: Satoshis = LnCurrencyUnits.toSatoshi(this)

  override def toPicoBitcoinValue: BigInt = this.toBigInt
}

object PicoSatoshis extends BaseNumbers[PicoSatoshis] {
  val min = PicoSatoshis(LnPolicy.minPicoSatoshis)
  val max = PicoSatoshis(LnPolicy.maxPicoSatoshis)
  val zero = PicoSatoshis(0)
  val one = PicoSatoshis(1)

  def apply(picoSatoshis: Int64): PicoSatoshis = PicoSatoshis(picoSatoshis.toBigInt)

  def apply(underlying: BigInt): PicoSatoshis = PicoSatoshisImpl(underlying)

  private case class PicoSatoshisImpl(underlying: BigInt) extends PicoSatoshis {
    require(underlying >= LnPolicy.minPicoSatoshis, "Number was too small for PicoSatoshis, got: " + underlying)
    require(underlying <= LnPolicy.maxPicoSatoshis, "Number was too big for PicoSatoshis, got: " + underlying)
  }
}

object LnCurrencyUnits {
  val oneMilliBTC: LnCurrencyUnit = MilliSatoshis.one
  val oneMicroBTC: LnCurrencyUnit = MicroSatoshis.one
  val oneNanoBTC: LnCurrencyUnit = NanoSatoshis.one
  val onePicoBTC: LnCurrencyUnit = PicoSatoshis.one
  val zero = PicoSatoshis(0)

  val milliMultiplier: BigDecimal = LnPolicy.milliMultiplier
  val microMultiplier: BigDecimal = LnPolicy.microMultiplier
  val nanoMultiplier: BigDecimal = LnPolicy.nanoMultiplier
  val picoMultiplier: BigDecimal = LnPolicy.picoMultiplier

  def toPicoBitcoinValue(lnCurrencyUnits: LnCurrencyUnit): BigInt = {
    lnCurrencyUnits.toBigInt * lnCurrencyUnits.toPicoBitcoinMultiplier
  }

  /**
   * For information regarding the rounding of sub-Satoshi values, please visit:
   * https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#commitment-transaction-outputs
   */
  def toSatoshi(lnCurrencyUnits: LnCurrencyUnit): Satoshis = {
    val sat = BigDecimal(lnCurrencyUnits.toBigInt) * Satoshis.one.satoshis.toBigDecimal * lnCurrencyUnits.multiplier
    val rounded = sat.setScale(0, RoundingMode.DOWN)
    if (rounded >= 1) {
      Satoshis(Int64(rounded.toBigIntExact().get))
    } else Satoshis.zero
  }

  def fromEncodedString(input: String): Try[LnCurrencyUnit] = {
    val currency = input.splitAt(input.length - 1)
    val amount = Try(BigInt(currency._1))
    if (amount.isSuccess) {
      val unit = currency._2
      unit match {
        case "m" => Try(MilliSatoshis(amount.get))
        case "u" => Try(MicroSatoshis(amount.get))
        case "n" => Try(NanoSatoshis(amount.get))
        case "p" => Try(PicoSatoshis(amount.get))
        case _ => Failure(new IllegalArgumentException(s"LnCurrencyUnit not found. Expected MilliSatoshis (m), MicroSatoshis (u), NanoSatoshis (n), or PicoSatoshis (p), got: $unit"))
      }
    } else { Failure(new IllegalArgumentException(s"Could not convert amount to valid number, got: $amount")) }
  }
}