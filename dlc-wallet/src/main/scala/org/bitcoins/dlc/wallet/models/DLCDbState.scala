package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.wallet.db.TransactionDb
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer
}
import org.bitcoins.core.protocol.dlc.models.{
  CETSignatures,
  ContractInfo,
  DLCFundingInput,
  DLCState
}
import org.bitcoins.dlc.wallet.util.DLCTxUtil

/** Super trait to represent the state of a DLC in the database */
sealed trait DLCDbState {
  def dlcDb: DLCDb
  def contractDataDb: DLCContractDataDb
  def contractInfo: ContractInfo
  def offerFundingInputsDb: Vector[DLCFundingInputDb]
  def offerDb: DLCOfferDb
  def offerPrevTxs: Vector[TransactionDb]

  def offerFundingInputs: Vector[DLCFundingInput] = {
    DLCTxUtil.matchPrevTxsWithInputs(offerFundingInputsDb, offerPrevTxs)
  }

  def offer: DLCOffer = {
    offerDb.toDLCOffer(contractInfo = contractInfo,
                       fundingInputs = offerFundingInputs,
                       dlcDb = dlcDb,
                       contractDataDb = contractDataDb)
  }

  final def state: DLCState = dlcDb.state
}

/** Represents a DLC in the database that
  * has not had its funding transaction published.
  * This means we are still setting up the DLC
  */
sealed trait DLCSetupDbState extends DLCDbState

/** Represents a DLC in the database that has
  * been fully setup and settled
  */
sealed trait DLCClosedDbState extends DLCDbState

case class OfferedDbState(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb])
    extends DLCSetupDbState {

  /** Converts a [[OfferedDbState]] to an [[AcceptDbState]]
    * @param acceptDb
    * @param acceptFundingInputsDb
    * @param acceptPrevTxsDb
    * @param cetSignaturesOpt the cet signatures, if we haven't pruned them from the database
    */
  def toAcceptDb(
      acceptDb: DLCAcceptDb,
      acceptFundingInputsDb: Vector[DLCFundingInputDb],
      acceptPrevTxsDb: Vector[TransactionDb],
      cetSigsOpt: Option[Vector[DLCCETSignaturesDb]],
      refundSigDb: DLCRefundSigsDb): AcceptDbState = {
    AcceptDbState(
      dlcDb = dlcDb,
      contractDataDb = contractDataDb,
      contractInfo = contractInfo,
      offerDb = offerDb,
      acceptDb = acceptDb,
      offerFundingInputsDb = offerFundingInputsDb,
      offerPrevTxs = offerPrevTxs,
      acceptFundingInputsDb = acceptFundingInputsDb,
      acceptPrevTxs = acceptPrevTxsDb,
      cetSigsOpt = cetSigsOpt,
      refundSigDb = refundSigDb
    )
  }
}

/** Shared data structured when we have all information to build a funding
  * transaction for a discreet log contract
  */
sealed trait CompleteSetupDLCDbState extends DLCSetupDbState {
  def dlcDb: DLCDb
  def contractDataDb: DLCContractDataDb
  def contractInfo: ContractInfo
  def offerDb: DLCOfferDb
  def acceptDb: DLCAcceptDb
  def offerFundingInputsDb: Vector[DLCFundingInputDb]
  def offerPrevTxs: Vector[TransactionDb]
  def acceptFundingInputsDb: Vector[DLCFundingInputDb]
  def acceptPrevTxs: Vector[TransactionDb]
  def refundSigDb: DLCRefundSigsDb
  def cetSigsOpt: Option[Vector[DLCCETSignaturesDb]]

  def allFundingInputs: Vector[DLCFundingInputDb]

  def acceptFundingInputs: Vector[DLCFundingInput] = {
    DLCTxUtil.matchPrevTxsWithInputs(acceptFundingInputsDb, acceptPrevTxs)
  }

  def acceptWithoutSigs: DLCAcceptWithoutSigs = {
    acceptDb.toDLCAcceptWithoutSigs(
      tempContractId = dlcDb.tempContractId,
      fundingInputs = acceptFundingInputs
    )
  }

  def cetSignaturesOpt: Option[CETSignatures] = {
    cetSigsOpt.map { cetSigs =>
      this match {
        case _: AcceptDbState =>
          acceptCETSigsOpt.get
        case _: SignDbState =>
          CETSignatures(cetSigs.map(c => (c.sigPoint, c.initiatorSig.get)))
      }
    }
  }

  def acceptCETSigsOpt: Option[CETSignatures] = {
    cetSigsOpt.map { cetSigs =>
      CETSignatures(cetSigs.map(c => (c.sigPoint, c.accepterSig)))
    }
  }

  def offererCETSigsOpt: Option[CETSignatures] = {
    cetSigsOpt.map { cetSigs =>
      CETSignatures(cetSigs.map(c => (c.sigPoint, c.initiatorSig.get)))
    }
  }

  /** Reconstructs the [[DLCAccept]] message if we have [[CETSignatures]]
    * in the database. If we don't have the signatures because we have pruned
    * them we return None as we can't reconstruct the message
    */
  def acceptOpt: Option[DLCAccept] = {
    acceptCETSigsOpt.map { cetSignatures =>
      acceptDb.toDLCAccept(dlcDb.tempContractId,
                           acceptFundingInputs,
                           outcomeSigs = cetSignatures.outcomeSigs,
                           refundSig = refundSigDb.accepterSig)
    }
  }
}

case class AcceptDbState(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    cetSigsOpt: Option[Vector[DLCCETSignaturesDb]],
    refundSigDb: DLCRefundSigsDb)
    extends CompleteSetupDLCDbState {

  override val allFundingInputs: Vector[DLCFundingInputDb] =
    offerFundingInputsDb ++ acceptFundingInputsDb

  val remotePrevTxs: Vector[TransactionDb] = {
    if (dlcDb.isInitiator) acceptPrevTxs
    else offerPrevTxs
  }

  val localPrevTxs: Vector[TransactionDb] = {
    if (dlcDb.isInitiator) offerPrevTxs
    else acceptPrevTxs
  }
}

case class SignDbState(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    refundSigDb: DLCRefundSigsDb,
    cetSigsOpt: Option[Vector[DLCCETSignaturesDb]]
) extends CompleteSetupDLCDbState {

  override val allFundingInputs: Vector[DLCFundingInputDb] =
    offerFundingInputsDb ++ acceptFundingInputsDb
}

case class ClosedDbStateWithCETSigs(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    refundSigsDb: DLCRefundSigsDb,
    cetSigs: Vector[DLCCETSignaturesDb]
) extends DLCClosedDbState {

  val allFundingInputs: Vector[DLCFundingInputDb] =
    offerFundingInputsDb ++ acceptFundingInputsDb
}

/** Sometimes we prune CET sigs from the database to save on disk space.
  * We need to handle this different than [[ClosedDbStateWithCETSigs]]
  */
case class ClosedDbStateNoCETSigs(
    dlcDb: DLCDb,
    contractDataDb: DLCContractDataDb,
    contractInfo: ContractInfo,
    offerDb: DLCOfferDb,
    acceptDb: DLCAcceptDb,
    offerFundingInputsDb: Vector[DLCFundingInputDb],
    offerPrevTxs: Vector[TransactionDb],
    acceptFundingInputsDb: Vector[DLCFundingInputDb],
    acceptPrevTxs: Vector[TransactionDb],
    refundSigsDb: DLCRefundSigsDb)
    extends DLCClosedDbState

object DLCClosedDbState {

  def fromAcceptSetupState(
      acceptState: AcceptDbState,
      cetSigsOpt: Option[Vector[DLCCETSignaturesDb]]): DLCClosedDbState = {
    cetSigsOpt match {
      case Some(cetSigs) =>
        ClosedDbStateWithCETSigs(
          acceptState.dlcDb,
          acceptState.contractDataDb,
          acceptState.contractInfo,
          acceptState.offerDb,
          acceptState.acceptDb,
          acceptState.offerFundingInputsDb,
          acceptState.offerPrevTxs,
          acceptState.acceptFundingInputsDb,
          acceptState.acceptPrevTxs,
          acceptState.refundSigDb,
          cetSigs
        )
      case None =>
        ClosedDbStateNoCETSigs(
          acceptState.dlcDb,
          acceptState.contractDataDb,
          acceptState.contractInfo,
          acceptState.offerDb,
          acceptState.acceptDb,
          acceptState.offerFundingInputsDb,
          acceptState.offerPrevTxs,
          acceptState.acceptFundingInputsDb,
          acceptState.acceptPrevTxs,
          acceptState.refundSigDb
        )
    }
  }

  def fromSignSetupState(
      signDbState: SignDbState,
      cetSigsOpt: Option[Vector[DLCCETSignaturesDb]]): DLCClosedDbState = {
    cetSigsOpt match {
      case Some(cetSigs) =>
        ClosedDbStateWithCETSigs(
          signDbState.dlcDb,
          signDbState.contractDataDb,
          signDbState.contractInfo,
          signDbState.offerDb,
          signDbState.acceptDb,
          signDbState.offerFundingInputsDb,
          signDbState.offerPrevTxs,
          signDbState.acceptFundingInputsDb,
          signDbState.acceptPrevTxs,
          signDbState.refundSigDb,
          cetSigs
        )
      case None =>
        ClosedDbStateNoCETSigs(
          signDbState.dlcDb,
          signDbState.contractDataDb,
          signDbState.contractInfo,
          signDbState.offerDb,
          signDbState.acceptDb,
          signDbState.offerFundingInputsDb,
          signDbState.offerPrevTxs,
          signDbState.acceptFundingInputsDb,
          signDbState.acceptPrevTxs,
          signDbState.refundSigDb
        )
    }
  }
}
