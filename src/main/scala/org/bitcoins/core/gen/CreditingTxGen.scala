package org.bitcoins.core.gen

import org.bitcoins.core.crypto.Sign
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.scalacheck.Gen

sealed abstract class CreditingTxGen {
  /** Minimum amount of outputs to generate */
  private val min = 1
  /** Maximum amount of outputs to generate */
  private val max = 3

  /** Note this generator does NOT generate outputs with negative values */
  private def nonEmptyOutputs: Gen[Seq[TransactionOutput]] = Gen.choose(1, 5).flatMap { n =>
    Gen.listOfN(n, TransactionGenerators.realisticOutput)
  }

  def rawOutput: Gen[CreditingTxGen.CreditingTxInfo] = {
    Gen.oneOf(p2pkOutput, p2pkhOutput, multiSigOutput, /*cltvOutput,*/ csvOutput)
  }

  def rawOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, rawOutput))

  def basicOutput: Gen[CreditingTxGen.CreditingTxInfo] = {
    Gen.oneOf(p2pkOutput, p2pkhOutput, multiSigOutput)
  }

  def nonP2WSHOutput: Gen[CreditingTxGen.CreditingTxInfo] = rawOutput

  def output: Gen[CreditingTxGen.CreditingTxInfo] = Gen.oneOf(
    p2pkOutput,
    p2pkhOutput, multiSigOutput, p2shOutput,
    csvOutput, /*cltvOutput,*/
    p2wpkhOutput, p2wshOutput)

  def outputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = {
    Gen.choose(min, 5).flatMap(n => Gen.listOfN(n, output))
  }

  /** Generates a crediting tx with a p2pk spk at the returned index */
  def p2pkOutput: Gen[CreditingTxGen.CreditingTxInfo] = ScriptGenerators.p2pkScriptPubKey.flatMap { p2pk =>
    build(p2pk._1, Seq(p2pk._2), None, None)
  }
  /** Generates multiple crediting txs with p2pk spks at the returned index */
  def p2pkOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkOutput))
  }

  /**
   * Generates a transaction that has a p2pkh output at the returned index. This
   * output can be spent by the returned ECPrivateKey
   */
  def p2pkhOutput: Gen[CreditingTxGen.CreditingTxInfo] = ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
    build(p2pkh._1, Seq(p2pkh._2), None, None)
  }

  /** Generates a sequence of p2pkh outputs at the returned index */
  def p2pkhOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkhOutput))
  }

  def multiSigOutput: Gen[CreditingTxGen.CreditingTxInfo] = ScriptGenerators.multiSigScriptPubKey.flatMap { multisig =>
    build(multisig._1, multisig._2, None, None)
  }

  def multiSigOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, multiSigOutput))
  }

  def p2shOutput: Gen[CreditingTxGen.CreditingTxInfo] = rawOutput.flatMap { o =>
    CryptoGenerators.hashType.map { hashType =>
      val oldTx = o._1
      val oldOutput = oldTx.outputs(o._2)
      val redeemScript = oldTx.outputs(o._2).scriptPubKey
      val p2sh = P2SHScriptPubKey(redeemScript)
      val updatedOutput = TransactionOutput(oldOutput.value, p2sh)
      val tx = oldTx match {
        case btx: BaseTransaction => BaseTransaction(btx.version, btx.inputs,
          btx.outputs.updated(o._2, updatedOutput), btx.lockTime)
        case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs,
          wtx.outputs.updated(o._2, updatedOutput), wtx.lockTime, wtx.witness)
      }
      (tx, o._2, o._3, Some(redeemScript), o._5, hashType)
    }
  }

  def p2shOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2shOutput))
  }

  def cltvOutput: Gen[CreditingTxGen.CreditingTxInfo] = TransactionGenerators.spendableCLTVValues.flatMap {
    case (scriptNum, _) =>
      basicOutput.flatMap { o =>
        CryptoGenerators.hashType.map { hashType =>
          val oldTx = o._1
          val oldOutput = oldTx.outputs(o._2)
          val csvSPK = CLTVScriptPubKey(scriptNum, oldOutput.scriptPubKey)
          val updatedOutput = TransactionOutput(oldOutput.value, csvSPK)
          val tx = oldTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, btx.inputs,
              btx.outputs.updated(o._2, updatedOutput), btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs,
              wtx.outputs.updated(o._2, updatedOutput), wtx.lockTime, wtx.witness)
          }
          (tx, o._2, o._3, o._4, o._5, hashType)
        }
      }
  }

  def cltvOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, cltvOutput))

  def csvOutput: Gen[CreditingTxGen.CreditingTxInfo] = TransactionGenerators.spendableCSVValues.flatMap {
    case (scriptNum, _) =>
      basicOutput.flatMap { o =>
        CryptoGenerators.hashType.map { hashType =>
          val oldTx = o._1
          val oldOutput = oldTx.outputs(o._2)
          val csvSPK = CSVScriptPubKey(scriptNum, oldOutput.scriptPubKey)
          val updatedOutput = TransactionOutput(oldOutput.value, csvSPK)
          val tx = oldTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, btx.inputs,
              btx.outputs.updated(o._2, updatedOutput), btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, wtx.inputs,
              wtx.outputs.updated(o._2, updatedOutput), wtx.lockTime, wtx.witness)
          }
          (tx, o._2, o._3, o._4, o._5, hashType)
        }
      }
  }

  def csvOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, csvOutput))

  def p2wpkhOutput: Gen[CreditingTxGen.CreditingTxInfo] = ScriptGenerators.p2wpkhSPKV0.flatMap { witSPK =>
    val scriptWit = P2WPKHWitnessV0(witSPK._2.head.publicKey)
    build(witSPK._1, witSPK._2, None, Some(scriptWit))
  }

  def p2wpkhOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2wpkhOutput))

  def p2wshOutput: Gen[CreditingTxGen.CreditingTxInfo] = nonP2WSHOutput.flatMap {
    case (tx, outputIndex, signer, redeemScriptOpt, scriptWitOpt, _) =>
      val spk = tx.outputs(outputIndex).scriptPubKey
      val scriptWit = P2WSHWitnessV0(spk)
      val witSPK = P2WSHWitnessSPKV0(spk)
      build(witSPK, signer, None, Some(scriptWit))
  }

  def p2wshOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2wshOutput))

  /** A nested output is a p2sh/p2wsh wrapped output */
  def nestedOutput: Gen[CreditingTxGen.CreditingTxInfo] = Gen.oneOf(p2wshOutput, p2shOutput)

  def nestedOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, nestedOutput))

  def random: Gen[CreditingTxGen.CreditingTxInfo] = nonEmptyOutputs.flatMap { outputs =>
    Gen.choose(0, outputs.size - 1).flatMap { outputIndex: Int =>
      ScriptGenerators.scriptPubKey.flatMap {
        case (spk, keys) =>
          WitnessGenerators.scriptWitness.flatMap { wit: ScriptWitness =>
            CryptoGenerators.hashType.map { hashType: HashType =>
              val tc = TransactionConstants
              val signers: Seq[Sign] = keys
              val creditingTx = BaseTransaction(tc.validLockVersion, Nil, outputs, tc.lockTime)
              (creditingTx, outputIndex, signers, Some(spk), Some(wit), hashType)
            }
          }
      }
    }
  }

  def randoms: Gen[Seq[CreditingTxGen.CreditingTxInfo]] = Gen.choose(min, max).flatMap(n => Gen.listOfN(n, random))

  private def build(spk: ScriptPubKey, signers: Seq[Sign],
    redeemScript: Option[ScriptPubKey],
    scriptWitness: Option[ScriptWitness]): Gen[CreditingTxGen.CreditingTxInfo] = nonEmptyOutputs.flatMap { outputs =>
    CryptoGenerators.hashType.flatMap { hashType =>
      Gen.choose(0, outputs.size - 1).map { idx =>
        val old = outputs(idx)
        val updated = outputs.updated(idx, TransactionOutput(old.value, spk))
        val tc = TransactionConstants
        val btx = BaseTransaction(tc.version, Nil, updated, tc.lockTime)
        val data = (btx, idx, signers, redeemScript, scriptWitness, hashType)
        data
      }
    }
  }

  /**
   * Generates a [[org.bitcoins.core.protocol.transaction.ZcashTransaction]] with a
   * valid [[P2PKHScriptPubKey]] for us to spend on the [[org.bitcoins.core.config.ZCashNetwork]]
   */
  def p2pkhOutputZcash: Gen[CreditingTxGen.CreditingTxInfoZcash] = ScriptGenerators.p2pkhScriptPubKey.flatMap { p2pkh =>
    buildZcash(p2pkh._1, Seq(p2pkh._2), None, None)
  }

  def p2pkhOutputsZcash: Gen[Seq[CreditingTxGen.CreditingTxInfoZcash]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, p2pkhOutputZcash))
  }

  /**
   * Generates a [[org.bitcoins.core.protocol.transaction.ZcashTransaction]] with a
   * valid [[MultiSignatureScriptPubKey]] for us to spend on the [[org.bitcoins.core.config.ZCashNetwork]]
   */
  def multiSigOutputZcash: Gen[CreditingTxGen.CreditingTxInfoZcash] = ScriptGenerators.multiSigScriptPubKey.flatMap { multisig =>
    buildZcash(multisig._1, multisig._2, None, None)
  }

  def multiSigOutputsZcash: Gen[Seq[CreditingTxGen.CreditingTxInfoZcash]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, multiSigOutputZcash))
  }

  /**
   * Generates valid [[org.bitcoins.core.protocol.transaction.ZcashTransaction]]'s with
   * outputs for us to spend on the [[org.bitcoins.core.config.ZCashNetwork]]
   * @return
   */
  def zCashOutput: Gen[CreditingTxGen.CreditingTxInfoZcash] = Gen.oneOf(
    p2pkhOutputZcash,
    multiSigOutputZcash)

  def zCashOutputs: Gen[Seq[CreditingTxGen.CreditingTxInfoZcash]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, zCashOutput))
  }

  /**
   * Generates a random [[org.bitcoins.core.protocol.transaction.ZcashTransaction]] for us
   * to attempt to spend in a property
   */
  def randomZcash: Gen[CreditingTxGen.CreditingTxInfoZcash] = nonEmptyOutputs.flatMap { outputs =>
    Gen.choose(0, outputs.size - 1).flatMap { outputIndex: Int =>
      ScriptGenerators.scriptPubKey.flatMap {
        case (spk, keys) =>
          WitnessGenerators.scriptWitness.flatMap { wit: ScriptWitness =>
            CryptoGenerators.hashType.map { hashType: HashType =>
              val tc = ZcashTxConstants
              val creditingTx = ZcashTransaction(tc.version, Nil, outputs, tc.lockTime).get
              (creditingTx, outputIndex, keys, Some(spk), Some(wit), hashType)
            }
          }
      }
    }
  }

  /**
   * Generates a sequence of random [[org.bitcoins.core.protocol.transaction.ZcashTransaction]]
   * that we can attempt to spend in a property
   */
  def randomsZcash: Gen[Seq[CreditingTxGen.CreditingTxInfoZcash]] = {
    Gen.choose(min, max).flatMap(n => Gen.listOfN(n, randomZcash))
  }
  private def buildZcash(spk: ScriptPubKey, signers: Seq[Sign],
    redeemScript: Option[ScriptPubKey],
    scriptWitness: Option[ScriptWitness]): Gen[CreditingTxGen.CreditingTxInfoZcash] = nonEmptyOutputs.flatMap { outputs =>
    CryptoGenerators.hashType.flatMap { hashType =>
      Gen.choose(0, outputs.size - 1).map { idx =>
        val old = outputs(idx)
        val updated = outputs.updated(idx, TransactionOutput(old.value, spk))
        val tc = ZcashTxConstants
        val ztx = ZcashTransaction(tc.version, Nil, updated, tc.lockTime).get
        val data = (ztx, idx, signers, redeemScript, scriptWitness, hashType)
        data
      }
    }
  }
}

object CreditingTxGen extends CreditingTxGen {
  type CreditingTxInfo = (Transaction, Int, Seq[Sign], Option[ScriptPubKey], Option[ScriptWitness], HashType)
  type CreditingTxInfoZcash = (ZcashTransaction, Int, Seq[Sign], Option[ScriptPubKey], Option[ScriptWitness], HashType)
}
