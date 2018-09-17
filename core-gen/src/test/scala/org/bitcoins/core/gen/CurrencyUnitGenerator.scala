package org.bitcoins.core.gen

import org.bitcoins.core.currency.{ Bitcoins, CurrencyUnit, CurrencyUnits, Satoshis }
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.ln._
import org.scalacheck.Gen

/**
 * Created by chris on 6/23/16.
 */
trait CurrencyUnitGenerator {

  def satoshis: Gen[Satoshis] = for {
    int64 <- NumberGenerator.int64s
  } yield Satoshis(int64)

  def bitcoins: Gen[Bitcoins] = for {
    sat <- satoshis
  } yield Bitcoins(sat)

  def currencyUnit: Gen[CurrencyUnit] = Gen.oneOf(satoshis, bitcoins)

  def positiveSatoshis: Gen[Satoshis] = satoshis.suchThat(_ >= CurrencyUnits.zero)

  /**
   * Generates a postiive satoshi value that is 'realistic'. This current 'realistic' range
   * is from 0 to 1,000,000 bitcoin
   */
  def positiveRealistic: Gen[Satoshis] = Gen.choose(0, Bitcoins(1000000).satoshis.toLong).map { n =>
    Satoshis(Int64(n))
  }
}

object CurrencyUnitGenerator extends CurrencyUnitGenerator

trait LnCurrencyUnitGenerator {

  def milliBitcoin: Gen[MilliSatoshis] = for {
    amount <- Gen.choose(MilliSatoshis.min.toLong, MilliSatoshis.max.toLong)
  } yield MilliSatoshis(amount)

  def microBitcoin: Gen[MicroSatoshis] = for {
    amount <- Gen.choose(MicroSatoshis.min.toLong, MicroSatoshis.max.toLong)
  } yield MicroSatoshis(amount)

  def nanoBitcoin: Gen[NanoSatoshis] = for {
    amount <- Gen.choose(NanoSatoshis.min.toLong, NanoSatoshis.max.toLong)
  } yield NanoSatoshis(amount)

  def picoBitcoin: Gen[PicoSatoshis] = for {
    amount <- Gen.choose(PicoSatoshis.min.toLong, PicoSatoshis.max.toLong)
  } yield PicoSatoshis(amount)

  def lnCurrencyUnit: Gen[LnCurrencyUnit] = Gen.oneOf(milliBitcoin, microBitcoin, nanoBitcoin, picoBitcoin)

  def negativeLnCurrencyUnit: Gen[LnCurrencyUnit] = lnCurrencyUnit.suchThat(_ < LnCurrencyUnits.zero)
}

object LnCurrencyUnitGenerator extends LnCurrencyUnitGenerator