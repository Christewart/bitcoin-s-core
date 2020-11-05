package org.bitcoins.core.protocol.tlv

import java.nio.charset.StandardCharsets

import org.bitcoins.core.number._
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.TLV.{
  DecodeTLVResult,
  FALSE_BYTE,
  TRUE_BYTE
}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.collection.immutable.NumericRange
import scala.math.Numeric.BigDecimalAsIfIntegral

sealed trait TLV extends NetworkElement {
  def tpe: BigSizeUInt
  def value: ByteVector

  def length: BigSizeUInt = {
    BigSizeUInt.calcFor(value)
  }

  override def bytes: ByteVector = {
    tpe.bytes ++ length.bytes ++ value
  }
}

sealed trait TLVParentFactory[T <: TLV] extends Factory[T] {

  def typeName: String

  def allFactories: Vector[TLVFactory[T]]

  lazy val knownTypes: Vector[BigSizeUInt] = allFactories.map(_.tpe)

  override def fromBytes(bytes: ByteVector): T = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None =>
        throw new IllegalArgumentException(s"Unknown $typeName type got $tpe")
    }
  }
}

object TLV extends TLVParentFactory[TLV] {

  val FALSE_BYTE: Byte = 0x00
  val TRUE_BYTE: Byte = 0x01

  case class DecodeTLVResult(
      tpe: BigSizeUInt,
      length: BigSizeUInt,
      value: ByteVector)

  def decodeTLV(bytes: ByteVector): DecodeTLVResult = {
    val tpe = BigSizeUInt(bytes)
    val length = BigSizeUInt(bytes.drop(tpe.byteSize))
    val prefixSize = tpe.byteSize + length.byteSize

    require(
      bytes.length >= prefixSize + length.num.toLong,
      s"Length specified was $length but not enough bytes in ${bytes.drop(prefixSize)}")

    val value = bytes.drop(prefixSize).take(length.num.toLong)

    DecodeTLVResult(tpe, length, value)
  }

  val typeName = "TLV"

  val allFactories: Vector[TLVFactory[TLV]] =
    Vector(ErrorTLV,
           PingTLV,
           PongTLV,
           OracleEventV0TLV,
           OracleAnnouncementV0TLV) ++ EventDescriptorTLV.allFactories

  // Need to override to be able to default to Unknown
  override def fromBytes(bytes: ByteVector): TLV = {
    val DecodeTLVResult(tpe, _, value) = decodeTLV(bytes)

    allFactories.find(_.tpe == tpe) match {
      case Some(tlvFactory) => tlvFactory.fromTLVValue(value)
      case None             => UnknownTLV(tpe, value)
    }
  }
}

sealed trait TLVFactory[+T <: TLV] extends Factory[T] {
  def tpe: BigSizeUInt
  def fromTLVValue(value: ByteVector): T

  override def fromBytes(bytes: ByteVector): T = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    require(tpe == this.tpe, s"Invalid type $tpe when expecting ${this.tpe}")

    fromTLVValue(value)
  }

  protected case class ValueIterator(value: ByteVector, var index: Int = 0) {

    def current: ByteVector = {
      value.drop(index)
    }

    def skip(numBytes: Long): Unit = {
      index += numBytes.toInt
      ()
    }

    def skip(bytes: NetworkElement): Unit = {
      skip(bytes.byteSize)
    }

    def take(numBytes: Int): ByteVector = {
      val bytes = current.take(numBytes)
      skip(numBytes)
      bytes
    }

    def takeBits(numBits: Int): ByteVector = {
      require(numBits % 8 == 0,
              s"Must take a round byte number of bits, got $numBits")
      take(numBytes = numBits / 8)
    }

    def takeBoolean(): Boolean = {
      take(1).head match {
        case FALSE_BYTE => false
        case TRUE_BYTE  => true
        case byte: Byte =>
          throw new RuntimeException(
            s"Boolean values must be 0x00 or 0x01, got $byte")
      }
    }

    def takeString(): String = {
      val size = BigSizeUInt(current)
      skip(size.byteSize)
      val strBytes = take(size.toInt)
      new String(strBytes.toArray, StandardCharsets.UTF_8)
    }

    def takeSPK(): ScriptPubKey = {
      val len = UInt16(takeBits(16)).toInt
      ScriptPubKey.fromAsmBytes(take(len))
    }
  }
}

case class UnknownTLV(tpe: BigSizeUInt, value: ByteVector) extends TLV {
  require(!TLV.knownTypes.contains(tpe), s"Type $tpe is known")
}

object UnknownTLV extends Factory[UnknownTLV] {

  override def fromBytes(bytes: ByteVector): UnknownTLV = {
    val DecodeTLVResult(tpe, _, value) = TLV.decodeTLV(bytes)

    UnknownTLV(tpe, value)
  }
}

/** @see [[https://github.com/lightningnetwork/lightning-rfc/blob/master/01-messaging.md#the-error-message]] */
case class ErrorTLV(id: ByteVector, data: ByteVector) extends TLV {
  require(id.length == 32, s"ID associated with error is incorrect length: $id")

  override val tpe: BigSizeUInt = ErrorTLV.tpe

  override val value: ByteVector = {
    id ++ UInt16(data.length).bytes ++ data
  }
}

object ErrorTLV extends TLVFactory[ErrorTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(17)

  override def fromTLVValue(value: ByteVector): ErrorTLV = {
    val id = value.take(32)
    val len = UInt16(value.drop(32).take(2))
    val data = value.drop(32 + 2).take(len.toInt)

    ErrorTLV(id, data)
  }
}

case class PingTLV(numPongBytes: UInt16, ignored: ByteVector) extends TLV {
  override val tpe: BigSizeUInt = PingTLV.tpe

  override val value: ByteVector = {
    numPongBytes.bytes ++ UInt16(ignored.length).bytes ++ ignored
  }
}

object PingTLV extends TLVFactory[PingTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(18)

  override def fromTLVValue(value: ByteVector): PingTLV = {
    val numPongBytes = UInt16(value.take(2))
    val numIgnored = UInt16(value.slice(2, 4))
    val ignored = value.drop(4).take(numIgnored.toLong)

    PingTLV(numPongBytes, ignored)
  }
}

case class PongTLV(ignored: ByteVector) extends TLV {
  override val tpe: BigSizeUInt = PongTLV.tpe

  override val value: ByteVector = {
    UInt16(ignored.length).bytes ++ ignored
  }
}

object PongTLV extends TLVFactory[PongTLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(19)

  override def fromTLVValue(value: ByteVector): PongTLV = {
    val numIgnored = UInt16(value.take(2))
    val ignored = value.drop(2).take(numIgnored.toLong)

    PongTLV.forIgnored(ignored)
  }

  def forIgnored(ignored: ByteVector): PongTLV = {
    new PongTLV(ignored)
  }
}

sealed trait EventDescriptorTLV extends TLV {

  def noncesNeeded: Int

  def outcomes: Vector[String]

}

object EventDescriptorTLV extends TLVParentFactory[EventDescriptorTLV] {

  val allFactories: Vector[TLVFactory[EventDescriptorTLV]] =
    Vector(EnumEventDescriptorV0TLV,
           RangeEventDescriptorV0TLV,
           DigitDecompositionEventDescriptorV0TLV)

  override def typeName: String = "EventDescriptorTLV"
}

/**
  * Describes an event over an enumerated set of outcomes
  * @param outcomes The set of possible outcomes
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/540c23a3e89c886814145cf16edfd48421d0175b/Oracle.md#simple-enumeration
  */
case class EnumEventDescriptorV0TLV(outcomes: Vector[String])
    extends EventDescriptorTLV {
  override def tpe: BigSizeUInt = EnumEventDescriptorV0TLV.tpe

  override val value: ByteVector = {
    val starting = UInt16(outcomes.size).bytes

    outcomes.foldLeft(starting) { (accum, outcome) =>
      val outcomeBytes = CryptoUtil.serializeForHash(outcome)
      accum ++ UInt16(outcomeBytes.length).bytes ++ outcomeBytes
    }
  }

  override def noncesNeeded: Int = 1
}

object EnumEventDescriptorV0TLV extends TLVFactory[EnumEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55302)

  override def fromTLVValue(value: ByteVector): EnumEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val count = UInt16(iter.takeBits(16))

    val builder = Vector.newBuilder[String]

    while (iter.index < value.length) {
      val len = UInt16(iter.takeBits(16))
      val outcomeBytes = iter.take(len.toInt)
      val str = new String(outcomeBytes.toArray, StandardCharsets.UTF_8)
      builder.+=(str)
    }

    val result = builder.result()

    require(count.toInt == result.size,
            "Did not parse the expected number of outcomes")

    EnumEventDescriptorV0TLV(result)
  }
}

trait NumericEventDescriptor { self: EventDescriptorTLV =>

  def stepDecimal: BigDecimal

  /** the maximum valid value a oracle can sign */
  def max: BigDecimal

  /** The minimum valid value in the oracle can sign */
  def min: BigDecimal

  /** The precision of the outcome representing the base exponent
    * by which to multiply the number represented by the composition
    * of the digits to obtain the actual outcome value
    */
  def precision: Int32

  /** All of outcomes in big decimal format. This is useful for applications doing things
    * based on the outcomes.
    * WARNING: For large ranges of outcomes, this can take a lot of memory. This collection that is returned
    * is eagerly evaluated rather than being lazy.
    * @see [[outcomes]] for strings that will get signed as by the oracle as an outcome
    */
  lazy val outcomesBigDec: Vector[BigDecimal] = {
    NumericRange
      .inclusive[BigDecimal](start = min, end = max, step = stepDecimal)(
        BigDecimalAsIfIntegral)
      .toVector
  }

  lazy val inverseStep: BigDecimal = 1 / stepDecimal

}

/**
  * Describes a simple event over a range of numbers
  * @param start The first number in the range
  * @param count The number of possible outcomes
  * @param step The increment between each outcome
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/540c23a3e89c886814145cf16edfd48421d0175b/Oracle.md#digit-decomposition
  */
case class RangeEventDescriptorV0TLV(
    start: Int32,
    count: UInt32,
    step: UInt16,
    unit: String,
    precision: Int32)
    extends EventDescriptorTLV
    with NumericEventDescriptor {
  override val stepDecimal: BigDecimal = BigDecimal(step.toInt)

  override val tpe: BigSizeUInt = RangeEventDescriptorV0TLV.tpe

  override val value: ByteVector = {
    val unitSize = BigSizeUInt(unit.length)
    val unitBytes = CryptoUtil.serializeForHash(unit)

    start.bytes ++ count.bytes ++ step.bytes ++
      unitSize.bytes ++ unitBytes ++ precision.bytes
  }

  override lazy val min: BigDecimal = BigDecimal(start.toInt)

  override lazy val max: BigDecimal = {
    val numSteps = count.toLong - 1
    val result = start.toBigInt + (numSteps * step.toLong)
    BigDecimal(result)
  }

  override lazy val outcomes: Vector[String] = {
    outcomesBigDec.map { real =>
      (real * inverseStep).toString()
    }
  }

  override def noncesNeeded: Int = 1
}

object RangeEventDescriptorV0TLV extends TLVFactory[RangeEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55304)

  override def fromTLVValue(value: ByteVector): RangeEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val start = Int32(iter.takeBits(32))
    val count = UInt32(iter.takeBits(32))
    val step = UInt16(iter.takeBits(16))

    val unit = iter.takeString()
    val precision = Int32(iter.takeBits(32))

    RangeEventDescriptorV0TLV(start, count, step, unit, precision)
  }
}

/** Describes a large range event using numerical decomposition
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/540c23a3e89c886814145cf16edfd48421d0175b/Oracle.md#digit-decomposition
  */
trait DigitDecompositionEventDescriptorV0TLV
    extends EventDescriptorTLV
    with NumericEventDescriptor {
  require(numDigits > UInt16.zero,
          s"Cannot have num digits be zero, got=${numDigits}")

  /** The base in which the outcome value is decomposed */
  def base: UInt16

  /** Whether the outcome can be negative */
  def isSigned: Boolean

  /** The number of digits that the oracle will sign */
  def numDigits: UInt16

  /** The unit of the outcome value */
  def unit: String

  override lazy val tpe: BigSizeUInt =
    DigitDecompositionEventDescriptorV0TLV.tpe

  override lazy val value: ByteVector = {
    val isSignedByte =
      if (isSigned) ByteVector(TRUE_BYTE) else ByteVector(FALSE_BYTE)

    val numDigitBytes = numDigits.bytes
    val unitSize = BigSizeUInt(unit.length)
    val unitBytes = CryptoUtil.serializeForHash(unit)

    base.bytes ++ isSignedByte ++ unitSize.bytes ++ unitBytes ++ precision.bytes ++ numDigitBytes
  }

  override def noncesNeeded: Int = {
    if (isSigned) numDigits.toInt + 1
    else numDigits.toInt
  }

  /** The maximum number in the large event range */
  def max: BigDecimal = {
    (Math.pow(base.toInt, numDigits.toInt).toLong - 1) * stepDecimal
  }

  /** the minimum number in the large event range */
  def min: BigDecimal = {
    if (isSigned) -max
    else 0
  }

  /** Useful for calculating the range out of outcomes.
    * This is how big of a "step" we take for each new value
    */
  override lazy val stepDecimal: BigDecimal = {
    Math.pow(base.toInt, precision.toInt)
  }

  /** Outcomes for a digit decomposition event
    * NOTE: Elements in this vector will be padded with leading '0's
    * so that the number of characters in the string is == [[numDigits]]
    * Example:
    * numDigits=2
    * base=10
    * isSigned=false
    * ["00","01","02"...,"09, "10","11" ... "98", "99"]
    */
  override lazy val outcomes: Vector[String] = {
    outcomesBigDec.map { real =>
      val base = real * inverseStep
      //https://stackoverflow.com/a/275715/967713
      DigitDecompositionEventDescriptorV0TLV.digitFormatter(base.toLongExact,
                                                            numDigits.toInt)
    }
  }
}

/** Represents a large range event that can be positive or negative */
case class SignedDigitDecompositionEventDescriptor(
    base: UInt16,
    numDigits: UInt16,
    unit: String,
    precision: Int32)
    extends DigitDecompositionEventDescriptorV0TLV {
  override val isSigned: Boolean = true
}

/** Represents a large range event that is unsigned */
case class UnsignedDigitDecompositionEventDescriptor(
    base: UInt16,
    numDigits: UInt16,
    unit: String,
    precision: Int32)
    extends DigitDecompositionEventDescriptorV0TLV {
  override val isSigned: Boolean = false
}

object DigitDecompositionEventDescriptorV0TLV
    extends TLVFactory[DigitDecompositionEventDescriptorV0TLV] {

  override val tpe: BigSizeUInt = BigSizeUInt(55306)

  override def fromTLVValue(
      value: ByteVector): DigitDecompositionEventDescriptorV0TLV = {
    val iter = ValueIterator(value)

    val base = UInt16(iter.takeBits(16))
    val isSigned = iter.takeBoolean()

    val unit = iter.takeString()
    val precision = Int32(iter.takeBits(32))
    val numDigits = UInt16(iter.takeBits(16))

    DigitDecompositionEventDescriptorV0TLV(base,
                                           isSigned,
                                           numDigits.toInt,
                                           unit,
                                           precision)
  }

  def apply(
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): DigitDecompositionEventDescriptorV0TLV = {
    if (isSigned) {
      SignedDigitDecompositionEventDescriptor(base,
                                              UInt16(numDigits),
                                              unit,
                                              precision)
    } else {
      UnsignedDigitDecompositionEventDescriptor(base,
                                                UInt16(numDigits),
                                                unit,
                                                precision)
    }
  }

  def digitFormatter(long: Long, numDigits: Int): String = {
    val prefix = if (long < 0) "-" else ""
    prefix + String.format(s"%0${numDigits}d", Math.abs(long))
  }
}

sealed trait OracleEventTLV extends TLV

case class OracleEventV0TLV(
    publicKey: SchnorrPublicKey,
    nonces: Vector[SchnorrNonce],
    eventMaturityEpoch: UInt32,
    eventDescriptor: EventDescriptorTLV,
    eventURI: String
) extends OracleEventTLV {

  require(eventDescriptor.noncesNeeded == nonces.size,
          "Not enough nonces for this event descriptor")

  override def tpe: BigSizeUInt = OracleEventV0TLV.tpe

  override val value: ByteVector = {
    val uriBytes = CryptoUtil.serializeForHash(eventURI)
    val numNonces = UInt16(nonces.size)
    val noncesBytes = nonces.foldLeft(numNonces.bytes)(_ ++ _.bytes)

    publicKey.bytes ++ noncesBytes ++ eventMaturityEpoch.bytes ++ eventDescriptor.bytes ++ uriBytes
  }
}

object OracleEventV0TLV extends TLVFactory[OracleEventV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55330)

  override def fromTLVValue(value: ByteVector): OracleEventV0TLV = {
    val iter = ValueIterator(value)

    val publicKey = SchnorrPublicKey(iter.take(32))

    val numNonces = UInt16(iter.takeBits(16))
    val builder = Vector.newBuilder[SchnorrNonce]

    for (_ <- 0 until numNonces.toInt) {
      val nonceBytes = iter.take(32)
      builder.+=(SchnorrNonce(nonceBytes))
    }

    val nonces = builder.result()

    require(
      numNonces.toInt == nonces.size,
      s"Did not parse the expected number of nonces expected ${numNonces.toInt}, got ${nonces.size}")

    val eventMaturity = UInt32(iter.takeBits(32))
    val eventDescriptor = EventDescriptorTLV(iter.current)
    iter.skip(eventDescriptor.byteSize)
    val eventURI = new String(iter.current.toArray, StandardCharsets.UTF_8)

    OracleEventV0TLV(publicKey,
                     nonces,
                     eventMaturity,
                     eventDescriptor,
                     eventURI)
  }
}

sealed trait OracleAnnouncementTLV extends TLV

case class OracleAnnouncementV0TLV(
    announcementSignature: SchnorrDigitalSignature,
    eventTLV: OracleEventV0TLV)
    extends OracleAnnouncementTLV {
  override def tpe: BigSizeUInt = OracleAnnouncementV0TLV.tpe

  override val value: ByteVector = announcementSignature.bytes ++ eventTLV.bytes
}

object OracleAnnouncementV0TLV extends TLVFactory[OracleAnnouncementV0TLV] {
  override val tpe: BigSizeUInt = BigSizeUInt(55332)

  override def fromTLVValue(value: ByteVector): OracleAnnouncementV0TLV = {
    val iter = ValueIterator(value)

    val sig = SchnorrDigitalSignature(iter.take(64))
    val eventTLV = OracleEventV0TLV(iter.current)

    OracleAnnouncementV0TLV(sig, eventTLV)
  }
}
