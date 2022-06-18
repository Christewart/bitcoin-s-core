package org.bitcoins.core.script.crypto

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto._
import org.bitcoins.core.protocol.script.{
  SigVersionBase,
  SigVersionTaprootKeySpend,
  SigVersionTapscript,
  SigVersionWitnessV0
}
import org.bitcoins.core.script._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control.{
  ControlOperationsInterpreter,
  OP_VERIFY
}
import org.bitcoins.core.script.flag.{
  ScriptFlagUtil,
  ScriptVerifyDiscourageUpgradablePubKeyType
}
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BitcoinScriptUtil
import org.bitcoins.crypto.{
  CryptoUtil,
  ECDigitalSignature,
  ECPublicKeyBytes,
  HashDigest,
  HashType,
  SchnorrDigitalSignature,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

import scala.util.{Failure, Success}

/** Created by chris on 1/6/16.
  */
sealed abstract class CryptoInterpreter {

  /** The input is hashed twice: first with SHA-256 and then with RIPEMD-160. */
  def opHash160(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_HASH160),
            "Script operation must be OP_HASH160")
    executeHashFunction(program, CryptoUtil.sha256Hash160(_: ByteVector))
  }

  /** The input is hashed using RIPEMD-160. */
  def opRipeMd160(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_RIPEMD160),
            "Script operation must be OP_RIPEMD160")
    executeHashFunction(program, CryptoUtil.ripeMd160(_: ByteVector))
  }

  /** The input is hashed using SHA-256. */
  def opSha256(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_SHA256),
            "Script operation must be OP_SHA256")
    executeHashFunction(program, CryptoUtil.sha256(_: ByteVector))
  }

  /** The input is hashed two times with SHA-256. */
  def opHash256(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_HASH256),
            "Script operation must be OP_HASH256")
    executeHashFunction(program, CryptoUtil.doubleSHA256(_: ByteVector))
  }

  /** The input is hashed using SHA-1. */
  def opSha1(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_SHA1),
            "Script top must be OP_SHA1")
    executeHashFunction(program, CryptoUtil.sha1(_: ByteVector))
  }

  /** The entire transaction's outputs, inputs, and script (from the most
    * recently-executed OP_CODESEPARATOR to the end) are hashed.
    * The signature used by
    * [[org.bitcoins.core.script.crypto.OP_CHECKSIG OP_CHECKSIG]]
    * must be a valid signature for this hash and public key.
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L880]]
    */
  def opCheckSig(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKSIG),
            "Script top must be OP_CHECKSIG")
    if (program.stack.size < 2) {
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val flags = program.flags
      val restOfStack = program.stack.tail.tail

      program.txSignatureComponent.sigVersion match {
        case SigVersionWitnessV0 | SigVersionWitnessV0 | SigVersionBase =>
          val pubKey = ECPublicKeyBytes(program.stack.head.bytes)
          val signature = ECDigitalSignature(program.stack.tail.head.bytes)
          val removedOpCodeSeparatorsScript =
            BitcoinScriptUtil.removeOpCodeSeparator(program)
          val result = TransactionSignatureChecker.checkSignature(
            program.txSignatureComponent,
            removedOpCodeSeparatorsScript,
            pubKey,
            signature,
            flags)
          handleSignatureValidation(program = program,
                                    result = result,
                                    restOfStack = restOfStack,
                                    numOpt = None)
        case SigVersionTapscript =>
          //drop control block + script bytes
          //https://github.com/bitcoin/bitcoin/blob/8ae4ba481ce8f7da173bef24432729c87a36cb70/src/script/interpreter.cpp#L1937

          val tapscriptE: Either[
            ScriptError,
            TransactionSignatureCheckerResult] = evalChecksigTapscript(program)

          tapscriptE match {
            case Left(err) =>
              if (
                err == ScriptErrorDiscourageUpgradablePubkeyType && !ScriptFlagUtil
                  .discourageUpgradablePublicKey(flags)
              ) {
                //trivially pass signature validation as required by BIP342
                //when the public key type is not known and the
                //SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_PUBKEYTYPE is NOT set
                handleSignatureValidation(program = program,
                                          result = SignatureValidationSuccess,
                                          restOfStack = restOfStack,
                                          numOpt = None)
              } else {
                program.failExecution(err)
              }

            case Right(result) =>
              handleSignatureValidation(program = program,
                                        result = result,
                                        restOfStack = restOfStack,
                                        numOpt = None)
          }

        case SigVersionTaprootKeySpend =>
          sys.error(s"Cannot use taproot keyspend with OP_CHECKSIG")

      }
    }
  }

  /** Gets the signature and hash type, returns None
    * if the signature is the empty byte vector which trivially
    * fails script interpreter validation
    */
  private def getSignatureAndHashType(
      stack: List[ScriptToken],
      isCheckSigAdd: Boolean): Option[(SchnorrDigitalSignature, HashType)] = {
    val sigBytes = {
      if (isCheckSigAdd) {
        stack(2).bytes
      } else {
        stack.tail.head.bytes
      }
    }
    val sigHashTypeOpt: Option[(SchnorrDigitalSignature, HashType)] = {
      if (sigBytes.length == 64) {
        val sig = SchnorrDigitalSignature.fromBytes(sigBytes)
        Some((sig, HashType.sigHashAll))
      } else if (sigBytes.length == 65) {
        val hashTypeByte = sigBytes.last
        val hashType = HashType.fromByte(hashTypeByte)
        val sig = SchnorrDigitalSignature.fromBytes(sigBytes.dropRight(1))
        Some((sig, hashType))
      } else if (sigBytes.isEmpty) {
        None
      } else {
        sys.error(
          s"Incorrect length for schnorr digital signature, got=${sigBytes.length}, expected 64 or 65 sigBytes=${sigBytes}")
      }
    }
    sigHashTypeOpt
  }

  private def evalChecksigTapscript(
      program: ExecutionInProgressScriptProgram): Either[
    ScriptError,
    TransactionSignatureCheckerResult] = {
    val stack = program.stack
    val pubKeyBytes = stack.head.bytes
    val schnorrPubKeyT =
      SchnorrPublicKey.fromBytesT(pubKeyBytes)
    //need to do weight validation
    if (pubKeyBytes.isEmpty) {
      //this failure catches two types of errors, if the pubkey is empty
      //and if its using an "upgraded" pubkey from a future soft fork
      //from bip342:
      //If the public key size is not zero and not 32 bytes, the public key is of an unknown public key type[6] and no actual signature verification is applied.
      //During script execution of signature opcodes they behave exactly as known public key types except that signature validation is considered to be successful.
      //see: https://github.com/bitcoin/bitcoin/blob/9e4fbebcc8e497016563e46de4c64fa094edab2d/src/script/interpreter.cpp#L374
      Left(ScriptErrorPubKeyType)
    } else if (
      program.flags.contains(
        ScriptVerifyDiscourageUpgradablePubKeyType) && schnorrPubKeyT.isFailure
    ) {
      Left(ScriptErrorDiscourageUpgradablePubkeyType)
    } else {
      val helperE: Either[ScriptError, TapscriptChecksigHelper] = {
        if (program.script.head != OP_CHECKSIGADD) {
          val sigHashTypeOpt = getSignatureAndHashType(stack, false)
          sigHashTypeOpt match {
            case Some((signature, hashType)) =>
              val restOfStack =
                program.stack.tail.tail //remove pubkey, signature
              val helper = TapscriptChecksigHelper(pubKey = schnorrPubKeyT.get,
                                                   signature = signature,
                                                   hashType = hashType,
                                                   restOfStack = restOfStack)
              Right(helper)
            case None =>
              //this is because the signature was empty
              Left(ScriptErrorEvalFalse)
          }

        } else if (program.script.head == OP_CHECKSIGADD) {
          val sigHashTypeOpt = getSignatureAndHashType(stack, true)

          sigHashTypeOpt match {
            case Some((signature, hashType)) =>
              val restOfStack =
                program.stack.tail.tail.tail //remove pubkey, num, signature
              val helper = TapscriptChecksigHelper(pubKey = schnorrPubKeyT.get,
                                                   signature,
                                                   hashType,
                                                   restOfStack)
              Right(helper)
            case None =>
              Left(ScriptErrorEvalFalse)
          }

        } else {
          sys.error(
            s"Can only run eval evalChecksigTapscript if script is at OP_CHECKSIGADD,OP_CHECKSIGVERIFY,OP_CHECKSIG")
        }
      }
      helperE match {
        case Right(helper) =>
          val result = TransactionSignatureChecker.checkSigTapscript(
            txSignatureComponent = program.txSignatureComponent,
            script = helper.restOfStack,
            pubKey = helper.pubKey,
            signature = helper.signature,
            hashType = helper.hashType,
            taprootOptions = program.taprootSerializationOptions,
            flags = program.flags
          )
          Right(result)
        case Left(err) => Left(err)
      }
    }
  }

  /** Runs [[org.bitcoins.core.script.crypto.OP_CHECKSIG OP_CHECKSIG]] with an
    * [[org.bitcoins.core.script.control.OP_VERIFY OP_VERIFY]] afterwards.
    */
  def opCheckSigVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKSIGVERIFY),
            "Script top must be OP_CHECKSIGVERIFY")
    if (program.stack.size < 2) {
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val newScript = OP_CHECKSIG :: OP_VERIFY :: program.script.tail
      val newProgram = program.updateScript(newScript)
      val programFromOpCheckSig = opCheckSig(newProgram)
      programFromOpCheckSig match {
        case _: ExecutedScriptProgram =>
          programFromOpCheckSig
        case p: ExecutionInProgressScriptProgram =>
          ControlOperationsInterpreter.opVerify(p)
      }
    }
  }

  /** All of the signature checking words will only match signatures to the data
    * after the most recently-executed
    * [[org.bitcoins.core.script.crypto.OP_CODESEPARATOR OP_CODESEPARATOR]].
    */
  def opCodeSeparator(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CODESEPARATOR),
            "Script top must be OP_CODESEPARATOR")

    val indexOfOpCodeSeparator =
      program.originalScript.size - program.script.size

    program
      .updateScript(program.script.tail)
      .updateLastCodeSeparator(indexOfOpCodeSeparator)
  }

  /** Compares the first signature against each public key until it finds an ECDSA match.
    * Starting with the subsequent public key, it compares the second signature against each remaining
    * public key until it finds an ECDSA match. The process is repeated until all signatures have been
    * checked or not enough public keys remain to produce a successful result.
    * All signatures need to match a public key.
    * Because public keys are not checked again if they fail any signature comparison,
    * signatures must be placed in the scriptSig using the same order as their corresponding public keys
    * were placed in the scriptPubKey or redeemScript. If all signatures are valid, 1 is returned, 0 otherwise.
    * Due to a bug, one extra unused value is removed from the stack.
    */
  final def opCheckMultiSig(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKMULTISIG),
            "Script top must be OP_CHECKMULTISIG")
    val flags = program.flags

    if (program.stack.size < 1) {
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      //these next lines remove the appropriate stack/script values after the signatures have been checked
      val nPossibleSignatures: ScriptNumber =
        BitcoinScriptUtil.numPossibleSignaturesOnStack(program)
      if (nPossibleSignatures < ScriptNumber.zero) {
        program.failExecution(ScriptErrorPubKeyCount)
      } else if (
        ScriptFlagUtil.requireMinimalData(
          flags) && !nPossibleSignatures.isShortestEncoding
      ) {
        program.failExecution(ScriptErrorUnknownError)
      } else if (program.stack.size < 2) {
        program.failExecution(ScriptErrorInvalidStackOperation)
      } else {
        val mRequiredSignatures: ScriptNumber =
          BitcoinScriptUtil.numRequiredSignaturesOnStack(program)

        if (
          ScriptFlagUtil.requireMinimalData(
            flags) && !mRequiredSignatures.isShortestEncoding
        ) {
          return program.failExecution(ScriptErrorUnknownError)
        }

        if (mRequiredSignatures < ScriptNumber.zero) {
          return program.failExecution(ScriptErrorSigCount)
        }
        val (pubKeysScriptTokens, stackWithoutPubKeys) =
          (program.stack.tail.slice(0, nPossibleSignatures.toInt),
           program.stack.tail
             .slice(nPossibleSignatures.toInt, program.stack.tail.size))

        val pubKeys =
          pubKeysScriptTokens.map(key => ECPublicKeyBytes(key.bytes))

        //+1 is for the fact that we have the # of sigs + the script token indicating the # of sigs
        val signaturesScriptTokens = program.stack.tail.slice(
          nPossibleSignatures.toInt + 1,
          nPossibleSignatures.toInt + mRequiredSignatures.toInt + 1)
        val signatures =
          signaturesScriptTokens.map(token => ECDigitalSignature(token.bytes))

        //this contains the extra Script OP that is required for OP_CHECKMULTISIG
        val stackWithoutPubKeysAndSignatures = stackWithoutPubKeys.tail
          .slice(mRequiredSignatures.toInt, stackWithoutPubKeys.tail.size)
        if (pubKeys.size > Consensus.maxPublicKeysPerMultiSig) {
          program.failExecution(ScriptErrorPubKeyCount)
        } else if (signatures.size > pubKeys.size) {
          program.failExecution(ScriptErrorSigCount)
        } else if (stackWithoutPubKeysAndSignatures.size < 1) {
          //this is because of a bug in bitcoin core for the implementation of OP_CHECKMULTISIG
          //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L966
          program.failExecution(ScriptErrorInvalidStackOperation)
        } else if (
          ScriptFlagUtil.requireNullDummy(flags) &&
          (stackWithoutPubKeysAndSignatures.nonEmpty && stackWithoutPubKeysAndSignatures.head.bytes.nonEmpty)
        ) {
          program.failExecution(ScriptErrorSigNullDummy)
        } else {
          //remove the last OP_CODESEPARATOR
          val removedOpCodeSeparatorsScript =
            BitcoinScriptUtil.removeOpCodeSeparator(program)
          val isValidSignatures: TransactionSignatureCheckerResult =
            TransactionSignatureChecker.multiSignatureEvaluator(
              program.txSignatureComponent,
              removedOpCodeSeparatorsScript,
              signatures,
              pubKeys,
              flags,
              mRequiredSignatures.toLong)

          //remove the extra OP_0 (null dummy) for OP_CHECKMULTISIG from the stack
          val restOfStack = stackWithoutPubKeysAndSignatures.tail
          handleSignatureValidation(program = program,
                                    result = isValidSignatures,
                                    restOfStack = restOfStack,
                                    numOpt = None)
        }
      }
    }
  }

  /** Runs
    * [[org.bitcoins.core.script.crypto.OP_CHECKMULTISIG OP_CHECKMULTISIG]] with an
    * [[org.bitcoins.core.script.control.OP_VERIFY OP_VERIFY]] afterwards
    */
  def opCheckMultiSigVerify(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(program.script.headOption.contains(OP_CHECKMULTISIGVERIFY),
            "Script top must be OP_CHECKMULTISIGVERIFY")
    if (program.stack.size < 3) {
      program.failExecution(ScriptErrorInvalidStackOperation)
    } else {
      val newScript = OP_CHECKMULTISIG :: OP_VERIFY :: program.script.tail
      val newProgram = program.updateScript(newScript)
      val programFromOpCheckMultiSig = opCheckMultiSig(newProgram)
      programFromOpCheckMultiSig match {
        case _: ExecutedScriptProgram =>
          programFromOpCheckMultiSig
        case p: ExecutionInProgressScriptProgram =>
          ControlOperationsInterpreter.opVerify(p)
      }
    }
  }

  def opCheckSigAdd(
      program: ExecutionInProgressScriptProgram): StartedScriptProgram = {
    require(
      program.script.headOption.contains(OP_CHECKSIGADD),
      s"Script top must be OP_CHECKSIGADD, got=${program.script.headOption}")
    program.txSignatureComponent.sigVersion match {
      case SigVersionBase | SigVersionWitnessV0 =>
        program.failExecution(ScriptErrorBadOpCode)
      case SigVersionTapscript | SigVersionTaprootKeySpend =>
        if (program.stack.length < 3) {
          program.failExecution(ScriptErrorInvalidStackOperation)
        } else {
          val flags = program.flags
          val requireMinimal =
            ScriptFlagUtil.requireMinimalData(program.flags)
          val numT = ScriptNumber(program.stack(1).bytes, requireMinimal)

          if (numT.isFailure) {
            throw numT.failed.get
          }
          val restOfStack =
            program.stack.tail.tail.tail //remove signature, num, pubkey

          val tapscriptE = evalChecksigTapscript(program)
          tapscriptE match {
            case Left(err) =>
              if (
                err == ScriptErrorDiscourageUpgradablePubkeyType && !ScriptFlagUtil
                  .discourageUpgradablePublicKey(flags)
              ) {
                //trivially pass signature validation as required by BIP342
                //when the public key type is not known and the
                //SCRIPT_VERIFY_DISCOURAGE_UPGRADABLE_PUBKEYTYPE is NOT set
                handleSignatureValidation(program = program,
                                          result = SignatureValidationSuccess,
                                          restOfStack = restOfStack,
                                          numOpt = Some(numT.get))
              } else if (err == ScriptErrorEvalFalse) {
                //means signature validation failed, don't increment the stack counter
                handleSignatureValidation(
                  program = program,
                  result = SignatureValidationErrorIncorrectSignatures,
                  restOfStack = restOfStack,
                  numOpt = Some(numT.get))
              } else {
                program.failExecution(err)
              }
            case Right(result) =>
              println(s"Right(result)=$result")
              handleSignatureValidation(program = program,
                                        result = result,
                                        restOfStack = restOfStack,
                                        numOpt = Some(numT.get))
          }
        }
    }
  }

  /** This is a higher order function designed to execute a hash function on the stack top of the program
    * For instance, we could pass in CryptoUtil.sha256 function as the `hashFunction` argument, which would then
    * apply sha256 to the stack top
    * @param program the script program whose stack top needs to be hashed
    * @param hashFunction the hash function which needs to be used on the stack top (sha256,ripemd160,etc..)
    * @return
    */
  private def executeHashFunction(
      program: ExecutionInProgressScriptProgram,
      hashFunction: ByteVector => HashDigest): StartedScriptProgram = {
    if (program.stack.nonEmpty) {
      val stackTop = program.stack.head
      val hash = ScriptConstant(hashFunction(stackTop.bytes).bytes)
      program.updateStackAndScript(hash :: program.stack.tail,
                                   program.script.tail)
    } else {
      program.failExecution(ScriptErrorInvalidStackOperation)
    }
  }

  /** Pushes the correct data onto the stack after signature validation
    * Pre-tapscript this meant just pushing OP_TRUE/OP_FALSE onto the stack
    * With tapscript and OP_CHECKSIGADD we increment the counter and
    * push it back onto the stack in case there are more signature operations
    * @param program
    * @param result
    * @param restOfStack
    * @param numOpt
    * @return
    */
  private def handleSignatureValidation(
      program: ExecutionInProgressScriptProgram,
      result: TransactionSignatureCheckerResult,
      restOfStack: Seq[ScriptToken],
      numOpt: Option[ScriptNumber]): StartedScriptProgram = {
    val pushOp = {
      numOpt match {
        case Some(num) =>
          if (result.isValid) {
            num.+(ScriptNumber.one)
          } else {
            num
          }
        case None =>
          if (result.isValid) {
            OP_TRUE
          } else {
            OP_FALSE
          }
      }
    }
    result match {
      case SignatureValidationSuccess =>
        //means that all of the signatures were correctly encoded and
        //that all of the signatures were valid signatures for the given
        //public keys
        program.updateStackAndScript(pushOp +: restOfStack, program.script.tail)
      case SignatureValidationErrorNotStrictDerEncoding =>
        //this means the script fails immediately
        //set the valid flag to false on the script
        //see BIP66 for more information on this
        //https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki#specification
        program.failExecution(ScriptErrorSigDer)
      case SignatureValidationErrorIncorrectSignatures =>
        //this means that signature verification failed, however all signatures were encoded correctly
        //just push a OP_FALSE onto the stack
        program.updateStackAndScript(pushOp +: restOfStack, program.script.tail)
      case SignatureValidationErrorSignatureCount =>
        //means that we did not have enough signatures for OP_CHECKMULTISIG
        program.failExecution(ScriptErrorInvalidStackOperation)
      case SignatureValidationErrorPubKeyEncoding =>
        //means that a public key was not encoded correctly
        program.failExecution(ScriptErrorPubKeyType)
      case SignatureValidationErrorHighSValue =>
        program.failExecution(ScriptErrorSigHighS)
      case SignatureValidationErrorHashType =>
        program.failExecution(ScriptErrorSigHashType)
      case SignatureValidationErrorWitnessPubKeyType =>
        program.failExecution(ScriptErrorWitnessPubKeyType)
      case SignatureValidationErrorNullFail =>
        program.failExecution(ScriptErrorSigNullFail)
    }
  }
}

object CryptoInterpreter extends CryptoInterpreter

case class TapscriptChecksigHelper(
    pubKey: SchnorrPublicKey,
    signature: SchnorrDigitalSignature,
    hashType: HashType,
    restOfStack: List[ScriptToken])
