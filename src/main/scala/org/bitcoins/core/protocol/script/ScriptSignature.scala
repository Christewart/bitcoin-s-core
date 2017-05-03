package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.serializers.script.{RawScriptSignatureParser, ScriptParser}
import org.bitcoins.core.util._

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 12/26/15.
  *
  */
sealed trait ScriptSignature extends NetworkElement {


  def compactSizeUInt = CompactSizeUInt.parseCompactSizeUInt(bytes)

  /**
    * Representation of a scriptSignature in a parsed assembly format
    * this data structure can be run through the script interpreter to
    * see if a script evaluates to true
    *
    * Note: The first byte(s) inside the byte array is the [[CompactSizeUInt]]
    * used to represent the size of the script serialization
    */
  lazy val asm : Seq[ScriptToken] = ScriptParser.fromBytes(bytes.splitAt(compactSizeUInt.size.toInt)._2)

  /** Byte vector for script program WITHOUT the [[CompactSizeUInt]], this is the raw byte vector that can be run */
  lazy val asmBytes = asm.flatMap(_.bytes)


  /**
    * The digital signatures contained inside of the script signature
    * p2pkh script signatures only have one sig
    * p2pk script signatures only have one sigs
    * p2sh script signatures can have m sigs
    * multisignature scripts can have m sigs
    */
  def signatures : Seq[ECDigitalSignature]

}

sealed trait NonStandardScriptSignature extends ScriptSignature {
  def signatures : Seq[ECDigitalSignature] = Nil
}

object NonStandardScriptSignature extends ScriptFactory[NonStandardScriptSignature] {
  private case class NonStandardScriptSignatureImpl(hex : String) extends NonStandardScriptSignature

  override def fromBytes(bytes: Seq[Byte]): NonStandardScriptSignature = {
    //make sure we can parse the bytes
    val asm = RawScriptSignatureParser.read(bytes).asm
    NonStandardScriptSignature.fromAsm(asm)
  }

  def fromAsm(asm : Seq[ScriptToken]): NonStandardScriptSignature = {
    buildScript(asm, NonStandardScriptSignatureImpl(_),{ _ => true}, "")
  }
}

/**
 * P2PKH script signatures have only one public key
 * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
 * P2PKH scriptSigs follow this format
 * <sig> <pubkey>
 */
sealed trait P2PKHScriptSignature extends ScriptSignature {

  /** P2PKH scriptSigs only have one signature */
  def signature : ECDigitalSignature = signatures.head

  /** Gives us the public key inside of a p2pkh script signature */
  def publicKey : ECPublicKey = ECPublicKey(asm.last.bytes)

  override def signatures : Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(asm(1).hex))
  }

}

object P2PKHScriptSignature extends ScriptFactory[P2PKHScriptSignature] {
  private case class P2PKHScriptSignatureImpl(hex : String) extends P2PKHScriptSignature

  override def fromBytes(bytes : Seq[Byte]): P2PKHScriptSignature = {
    val asm = RawScriptSignatureParser.read(bytes).asm
    P2PKHScriptSignature.fromAsm(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptSignature = {
    buildScript(asm, P2PKHScriptSignatureImpl(_),isP2PKHScriptSig(_), "Given asm was not a P2PKHScriptSignature, got: " + asm)
  }

  /**
    * Builds a script signature from a digital signature and a public key
    * this is a pay to public key hash script sig */
  def apply(signature : ECDigitalSignature, pubKey : ECPublicKey) : P2PKHScriptSignature = {
    val signatureBytesToPushOntoStack = BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val pubKeyBytesToPushOntoStack = BitcoinScriptUtil.calculatePushOp(pubKey.bytes)
    val asm : Seq[ScriptToken] = signatureBytesToPushOntoStack ++ Seq(ScriptConstant(signature.hex)) ++
      pubKeyBytesToPushOntoStack ++ Seq(ScriptConstant(pubKey.hex))
    fromAsm(asm)
  }

  /** Determines if the given asm matches a [[P2PKHScriptSignature]] */
  def isP2PKHScriptSig(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(w : BytesToPushOntoStack, x : ScriptConstant, y : BytesToPushOntoStack,
      z : ScriptConstant) =>
      if (ECPublicKey.isFullyValid(z.bytes)) true
      else !P2SHScriptSignature.isRedeemScript(z)
    case _ => false
  }
}

/**
 * Represents a pay-to-script-hash script signature
 * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
 * P2SH scriptSigs have the following format
 * <sig> [sig] [sig...] <redeemScript>
 */
sealed trait P2SHScriptSignature extends ScriptSignature {

  /** The redeemScript represents the conditions that must be satisfied to spend the output */
  def redeemScript : ScriptPubKey = {
    //for P2SH(P2WSH) the entire scriptSig asm is technically the redeem script
    //see BIP141
    WitnessScriptPubKey(asm).getOrElse(ScriptPubKey(ScriptParser.fromBytes(asm.last.bytes)))
  }


  /** Returns the script signature of this p2shScriptSig with no serialized redeemScript */
  def scriptSignatureNoRedeemScript: ScriptSignature = {
    //witness scriptPubKeys always have EmptyScriptSigs
    if (WitnessScriptPubKey.isWitnessScriptPubKey(asm)) EmptyScriptSignature
    else {
      val asmWithoutRedeemScriptAndPushOp = asm(asm.size - 2) match {
        case b : BytesToPushOntoStack => asm.dropRight(2)
        case _ => asm.dropRight(3)
      }
      ScriptSignature.fromAsm(asmWithoutRedeemScriptAndPushOp)
    }
  }


  /** Returns the public keys for the p2sh scriptSignature */
  def publicKeys : Seq[ECPublicKey] = {
    val pubKeys : Seq[ScriptToken] = redeemScript.asm.filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
    pubKeys.map(k => ECPublicKey(k.hex))
  }


  /** The digital signatures inside of the scriptSig */
  def signatures : Seq[ECDigitalSignature] = {
    val nonRedeemScript = splitAtRedeemScript(asm)._1
    val sigs = nonRedeemScript.filter(_.isInstanceOf[ScriptConstant]).filterNot(_.isInstanceOf[ScriptNumberOperation]).filterNot(_.hex.length < 100)
    sigs.map(s => ECDigitalSignature(s.hex))
  }


  /**
    * Splits the given asm into two parts
    * the first part is the digital signatures
    * the second part is the redeem script */
  def splitAtRedeemScript(asm : Seq[ScriptToken]) : (Seq[ScriptToken],Seq[ScriptToken]) = {
    (scriptSignatureNoRedeemScript.asm, redeemScript.asm)
  }
}

object P2SHScriptSignature extends ScriptFactory[P2SHScriptSignature]  {
  private case class P2SHScriptSignatureImpl(hex : String) extends P2SHScriptSignature

  override def fromBytes(bytes : Seq[Byte]): P2SHScriptSignature = {
    val asm = RawScriptSignatureParser.read(bytes).asm
    P2SHScriptSignature.fromAsm(asm)
  }

  def apply(scriptSig : ScriptSignature, redeemScript : ScriptPubKey): P2SHScriptSignature = {
    //we need to calculate the size of the redeemScript and add the corresponding push op
    val serializedRedeemScript = ScriptConstant(redeemScript.asmBytes)
    val pushOps = BitcoinScriptUtil.calculatePushOp(serializedRedeemScript)
    val asm: Seq[ScriptToken] = scriptSig.asm ++ pushOps ++ Seq(serializedRedeemScript)
    fromAsm(asm)
  }

  def apply(witnessScriptPubKey: WitnessScriptPubKey): P2SHScriptSignature = {
    P2SHScriptSignature(EmptyScriptSignature,witnessScriptPubKey)
  }


  def fromAsm(asm: Seq[ScriptToken]): P2SHScriptSignature = {
    //everything can be a P2SHScriptSignature, thus passing the trivially true function
    //the most important thing to note is we cannot have a P2SHScriptSignature unless
    //we have a P2SHScriptPubKey
    //previously P2SHScriptSignature's redeem script had to be standard scriptPubKey's, this
    //was removed in 0.11 or 0.12 of Bitcoin Core
    buildScript(asm, P2SHScriptSignatureImpl(_),{ _ => true}, "Given asm tokens are not a p2sh scriptSig, got: " + asm)
  }

  /** Tests if the given asm tokens are a [[P2SHScriptSignature]] */
  def isP2SHScriptSig(asm: Seq[ScriptToken]): Boolean = asm match {
    case _ if asm.size > 1 && isRedeemScript(asm.last) => true
    case _ if WitnessScriptPubKey.isWitnessScriptPubKey(asm) => true
    case _ => false
  }

  /** Detects if the given script token is a redeem script */
  def isRedeemScript(token : ScriptToken) : Boolean = {
    val redeemScriptTry : Try[ScriptPubKey] = parseRedeemScript(token)
    redeemScriptTry match {
      case Success(redeemScript) =>
        redeemScript match {
          case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey
               | _: P2SHScriptPubKey | _: P2PKScriptPubKey
               | _: CLTVScriptPubKey | _: CSVScriptPubKey
               | _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey
               | _: EscrowTimeoutScriptPubKey => true
          case _: NonStandardScriptPubKey | _: WitnessCommitment => false
          case EmptyScriptPubKey => false
        }
      case Failure(_) => false
    }
  }


  /** Parses a redeem script from the given script token */
  def parseRedeemScript(scriptToken : ScriptToken) : Try[ScriptPubKey] = {
    val asm = ScriptParser.fromBytes(scriptToken.bytes)
    logger.debug("Asm for redeem script: " + asm)
    val redeemScript : Try[ScriptPubKey] = Try(ScriptPubKey(asm))
    redeemScript
  }
}

/**
 * Represents a multisignature script signature
 * https://bitcoin.org/en/developer-guide#multisig
 * Multisig script sigs have the following format
 * OP_0 <A sig> [B sig] [C sig...]
 */
sealed trait MultiSignatureScriptSignature extends ScriptSignature {

  /** The digital signatures inside of the scriptSig */
  def signatures : Seq[ECDigitalSignature] = {
    asm.tail.filter(_.isInstanceOf[ScriptConstant])
      .map(sig => ECDigitalSignature(sig.hex))
  }
}

object MultiSignatureScriptSignature extends ScriptFactory[MultiSignatureScriptSignature] {

  private case class MultiSignatureScriptSignatureImpl(hex : String) extends MultiSignatureScriptSignature

  override def fromBytes(bytes : Seq[Byte]): MultiSignatureScriptSignature = {
    val asm = RawScriptSignatureParser.read(bytes).asm
    MultiSignatureScriptSignature.fromAsm(asm)
  }

  def apply(signatures : Seq[ECDigitalSignature]): MultiSignatureScriptSignature = {
    val sigsPushOpsPairs : Seq[Seq[ScriptToken]] = for {
      sig <- signatures
      constant = ScriptConstant(sig.bytes)
      pushOps = BitcoinScriptUtil.calculatePushOp(sig.bytes)
    } yield pushOps ++ Seq(constant)
    val sigsWithPushOps = sigsPushOpsPairs.flatten
    //OP_0 is for the dummy input required by OP_CHECKMULTISIG
    val asm = OP_0 +: sigsWithPushOps
    MultiSignatureScriptSignature.fromAsm(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): MultiSignatureScriptSignature = {
    buildScript(asm, MultiSignatureScriptSignatureImpl(_),isMultiSignatureScriptSignature(_),
      "The given asm tokens were not a multisignature script sig: " + asm)
  }

  /**
    * Checks if the given script tokens are a multisignature script sig
    * format: OP_0 <A sig> [B sig] [C sig...]
    *
    * @param asm the asm to check if it falls in the multisignature script sig format
    * @return boolean indicating if the scriptsignature is a multisignature script signature
    */
  def isMultiSignatureScriptSignature(asm : Seq[ScriptToken]) : Boolean = asm.isEmpty match {
    case true => false
    //case false if (asm.size == 1) => false
    case false =>
      val firstTokenIsScriptNumberOperation = asm.head.isInstanceOf[ScriptNumberOperation]
      val restOfScriptIsPushOpsOrScriptConstants = asm.tail.map(
        token => token.isInstanceOf[ScriptConstant] || StackPushOperationFactory.isPushOperation(token)
      ).exists(_ == false)
      firstTokenIsScriptNumberOperation && !restOfScriptIsPushOpsOrScriptConstants
  }
}

/**
 * Represents a pay to public key script signature
 * https://bitcoin.org/en/developer-guide#pubkey
 * Signature script: <sig>
 */
sealed trait P2PKScriptSignature extends ScriptSignature {

  /** PubKey scriptSignatures only have one signature */
  def signature : ECDigitalSignature = signatures.head

  /** The digital signatures inside of the scriptSig */
  def signatures : Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(BitcoinScriptUtil.filterPushOps(asm).head.hex))
  }
}

object P2PKScriptSignature extends ScriptFactory[P2PKScriptSignature] {
  private case class P2PKScriptSignatureImpl(hex : String) extends P2PKScriptSignature

  def apply(signature: ECDigitalSignature): P2PKScriptSignature = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val signatureConstant = ScriptConstant(signature.bytes)
    val asm = pushOps ++ Seq(signatureConstant)
    P2PKScriptSignature.fromAsm(asm)
  }

  def fromAsm(asm: Seq[ScriptToken]): P2PKScriptSignature = {
    buildScript(asm, P2PKScriptSignatureImpl(_),isP2PKScriptSignature(_),
      "The given asm tokens were not a p2pk script sig: " + asm)
  }

  override def fromBytes(bytes: Seq[Byte]): P2PKScriptSignature = {
    val asm = RawScriptSignatureParser.read(bytes).asm
    P2PKScriptSignature.fromAsm(asm)
  }

  /** P2PK scriptSigs always have the pattern [pushop, digitalSignature] */
  def isP2PKScriptSignature(asm: Seq[ScriptToken]): Boolean = asm match {
    case List(w : BytesToPushOntoStack, x : ScriptConstant) => true
    case _ => false
  }
}

/** Parent type for all lock time script signatures, these spend [[LockTimeScriptPubKey]] */
sealed trait LockTimeScriptSignature extends ScriptSignature {
  def scriptSig : ScriptSignature = ScriptSignature(hex)

  override def signatures : Seq[ECDigitalSignature] = scriptSig.signatures
}

sealed trait CLTVScriptSignature extends LockTimeScriptSignature

object CLTVScriptSignature extends Factory[CLTVScriptSignature] {
  private case class CLTVScriptSignatureImpl(hex : String) extends CLTVScriptSignature

  override def fromBytes(bytes : Seq[Byte]) : CLTVScriptSignature = {
    val hex = BitcoinSUtil.encodeHex(bytes)
    fromHex(hex)
  }

  override def fromHex(hex : String) : CLTVScriptSignature = {
    CLTVScriptSignatureImpl(hex)
  }

  def apply(scriptSig : ScriptSignature) : CLTVScriptSignature = {
    fromHex(scriptSig.hex)
  }

}

sealed trait CSVScriptSignature extends LockTimeScriptSignature

object CSVScriptSignature extends Factory[CSVScriptSignature] {
  private case class CSVScriptSignatureImpl(hex : String) extends CSVScriptSignature

  override def fromBytes(bytes : Seq[Byte]) : CSVScriptSignature = {
    val hex = BitcoinSUtil.encodeHex(bytes)
    fromHex(hex)
  }

  override def fromHex(hex : String) : CSVScriptSignature = {
    CSVScriptSignatureImpl(hex)
  }

  def apply(scriptSig : ScriptSignature) : CSVScriptSignature = {
    fromHex(scriptSig.hex)
  }
}


/** Represents the empty script signature */
case object EmptyScriptSignature extends ScriptSignature {
  def signatures = Nil
  def hex = "00"
}

object ScriptSignature extends Factory[ScriptSignature] with BitcoinSLogger {


  /** Returns an empty script signature */
  def empty : ScriptSignature = EmptyScriptSignature

  def fromBytes(bytes : Seq[Byte]) : ScriptSignature = RawScriptSignatureParser.read(bytes)

  /** Creates a scriptSignature from the list of script tokens */
  def fromAsm(tokens : Seq[ScriptToken]) : ScriptSignature = tokens match {
    case Nil => EmptyScriptSignature
    case _  if (tokens.size > 1 && P2SHScriptSignature.isRedeemScript(tokens.last)) =>
      P2SHScriptSignature.fromAsm(tokens)
    case _ if (MultiSignatureScriptSignature.isMultiSignatureScriptSignature(tokens)) =>
      MultiSignatureScriptSignature.fromAsm(tokens)
    case _ if P2PKHScriptSignature.isP2PKHScriptSig(tokens) => P2PKHScriptSignature.fromAsm(tokens)
    case _ if P2PKScriptSignature.isP2PKScriptSignature(tokens) => P2PKScriptSignature.fromAsm(tokens)
    case _ => NonStandardScriptSignature.fromAsm(tokens)
  }


  /**
    * Creates a script signature from the given tokens and scriptPubKey
    * @param tokens the script signature's tokens
    * @param scriptPubKey the scriptPubKey which the script signature is trying to spend
    * @return
    */
  def fromScriptPubKey(tokens : Seq[ScriptToken], scriptPubKey : ScriptPubKey) : ScriptSignature = scriptPubKey match {
    case _: P2SHScriptPubKey => P2SHScriptSignature.fromAsm(tokens)
    case _: P2PKHScriptPubKey => P2PKHScriptSignature.fromAsm(tokens)
    case _: P2PKScriptPubKey => P2PKScriptSignature.fromAsm(tokens)
    case _: MultiSignatureScriptPubKey => MultiSignatureScriptSignature.fromAsm(tokens)
    case _: NonStandardScriptPubKey => NonStandardScriptSignature.fromAsm(tokens)
    case s : CLTVScriptPubKey => fromScriptPubKey(tokens, s.nestedScriptPubKey)
    case s : CSVScriptPubKey => fromScriptPubKey(tokens, s.nestedScriptPubKey)
    case escrowWithTimeout : EscrowTimeoutScriptPubKey =>
      val isMultSig = BitcoinScriptUtil.castToBool(tokens.head)
      if (isMultSig) MultiSignatureScriptSignature.fromAsm(tokens.tail)
      else EscrowTimeoutScriptSignature.fromAsm(tokens,escrowWithTimeout)
    case _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey  => EmptyScriptSignature
    case EmptyScriptPubKey => if (tokens.isEmpty) EmptyScriptSignature else NonStandardScriptSignature.fromAsm(tokens)
    case _ : WitnessCommitment => throw new IllegalArgumentException("Cannot spend witness commitment scriptPubKey")
  }

  def apply(tokens : Seq[ScriptToken], scriptPubKey : ScriptPubKey) : ScriptSignature = fromScriptPubKey(tokens, scriptPubKey)
}

/** [[ScriptSignature]] that spends a [[EscrowTimeoutScriptPubKey]], the underlying script signature can be
  * a [[MultiSignatureScriptSignature]] or a [[CSVScriptSignature]] as those are te two underlying scripts
  * of a [[EscrowTimeoutScriptPubKey]]
  *
  * If the last element of the [[asm]] evaluates to true, it is a scriptsig that attempts to spend the escrow
  * if the last element of the [[asm]] evaluates to false, it is a scriptsig that attempts to spend the timeout
  * */
sealed trait EscrowTimeoutScriptSignature extends ScriptSignature {
  def scriptSig: ScriptSignature = ScriptSignature(hex)
  override def signatures = scriptSig.signatures
  override def toString = "EscrowWithTimeoutScriptSignature(" + scriptSig + ")"

  /** Checks if the given asm fulfills the timeout or escrow of the [[EscrowTimeoutScriptPubKey]] */
  def isEscrow: Boolean = BitcoinScriptUtil.castToBool(asm.last)

  def isTimeout: Boolean = !isEscrow
}


object EscrowTimeoutScriptSignature extends Factory[EscrowTimeoutScriptSignature] {
  private case class EscrowTimeoutScriptSignatureImpl(hex: String) extends EscrowTimeoutScriptSignature

  override def fromBytes(bytes: Seq[Byte]): EscrowTimeoutScriptSignature = {
    EscrowTimeoutScriptSignatureImpl(BitcoinSUtil.encodeHex(bytes))
  }

  def fromAsm(asm: Seq[ScriptToken], scriptPubKey: EscrowTimeoutScriptPubKey): EscrowTimeoutScriptSignature = {
    require(isValidEscrowTimeoutScriptSig(asm,scriptPubKey), "Given asm was not a EscrowWithTimeoutScriptSignature, got: " + asm)
    val asmHex = asm.map(_.hex).mkString
    val c = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
    val fullHex = c.hex + asmHex
    fromHex(fullHex)
  }

  def fromAsm(asm: Seq[ScriptToken]): EscrowTimeoutScriptSignature = {
    require(asm.nonEmpty)
    val nested = asm.take(asm.length - 1)
    val nestedScriptSig = if (BitcoinScriptUtil.castToBool(asm.last)) {
      require(MultiSignatureScriptSignature.isMultiSignatureScriptSignature(nested), "Need multisigScriptSig, got: " + nested)
      MultiSignatureScriptSignature.fromAsm(nested)
    } else {
      CSVScriptSignature(ScriptSignature.fromAsm(nested))
    }
    val bytes = nestedScriptSig.asmBytes ++ asm.last.bytes
    val c = CompactSizeUInt.calculateCompactSizeUInt(bytes)
    val fullBytes = c.bytes ++ bytes
    fromBytes(fullBytes)
  }

  def isValidEscrowTimeoutScriptSig(asm: Seq[ScriptToken],
                                                       scriptPubKey: EscrowTimeoutScriptPubKey): Boolean = {
    val nested = asm.take(asm.length - 1)
    val isMultiSig = BitcoinScriptUtil.castToBool(asm.last)
    if (isMultiSig) {
      MultiSignatureScriptSignature.isMultiSignatureScriptSignature(nested)
    } else {
      val locktimeScript = scriptPubKey.timeout.nestedScriptPubKey
      Try(ScriptSignature(nested,locktimeScript)).isSuccess
    }
  }

  def apply(scriptSig: ScriptSignature): Try[EscrowTimeoutScriptSignature] = scriptSig match {
    case m: MultiSignatureScriptSignature => Success(fromMultiSig(m))
    case lock: LockTimeScriptSignature => Success(fromLockTime(lock))
    case x @ (_: P2PKScriptSignature | _: P2PKHScriptSignature | _: P2SHScriptSignature | _:NonStandardScriptSignature
      | _: EscrowTimeoutScriptSignature | EmptyScriptSignature) => Failure(new IllegalArgumentException("Cannot create a EscrowTimeoutScriptSignature out of " + x))

  }

  /** Creates a [[org.bitcoins.core.protocol.script.EscrowTimeoutScriptSignature]] that spends the escrow
    * branch of a [[EscrowTimeoutScriptPubKey]] */
  def fromMultiSig(multiSigScriptSig: MultiSignatureScriptSignature): EscrowTimeoutScriptSignature = {
    val asm = multiSigScriptSig.asm ++ Seq(OP_1)
    fromAsm(asm)
  }

  /** Creates a [[org.bitcoins.core.protocol.script.EscrowTimeoutScriptSignature]] that spends the locktime branch
    * of the [[EscrowTimeoutScriptPubKey]] */
  def fromLockTime(l: LockTimeScriptSignature): EscrowTimeoutScriptSignature = {
    val asm = l.asm ++ Seq(OP_0)
    fromAsm(asm)
  }
}
