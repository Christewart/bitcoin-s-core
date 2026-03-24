package org.bitcoins.spark.rpc

import com.google.protobuf.empty.Empty
import org.apache.pekko.NotUsed
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.apache.pekko.stream.scaladsl.Source
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.spark.rpc.proto.spark._
import org.bitcoins.spark.rpc.proto.spark.authn._
import org.bitcoins.spark.rpc.proto.spark.token._

import scala.concurrent.{ExecutionContext, Future}

/** A client for the Spark RPC server
  */
case class SparkRpcClient(instance: SparkInstance)(implicit
    val system: ActorSystem
) extends StartStopAsync[SparkRpcClient] {

  implicit val ec: ExecutionContext = system.dispatcher

  private val settings = GrpcClientSettings
    .connectToServiceAt(
      instance.rpcUri.getHost,
      instance.rpcUri.getPort
    )
    .withTls(instance.rpcUri.getScheme == "https")

  // Assuming generated client name
  private val sparkClient = SparkServiceClient(settings)
  private val sparkAuthnClient = SparkAuthnServiceClient(settings)
  private val sparkTokenClient = SparkTokenServiceClient(settings)

  override def start(): Future[SparkRpcClient] = Future.successful(this)

  override def stop(): Future[SparkRpcClient] = {
    for {
      _ <- sparkClient.close()
      _ <- sparkAuthnClient.close()
      _ <- sparkTokenClient.close()
    } yield this
  }

  def generateDepositAddress(
      request: GenerateDepositAddressRequest
  ): Future[GenerateDepositAddressResponse] = {
    sparkClient.generate_deposit_address(request)
  }

  def generateStaticDepositAddress(
      request: GenerateStaticDepositAddressRequest
  ): Future[GenerateStaticDepositAddressResponse] = {
    sparkClient.generate_static_deposit_address(request)
  }

  def rotateStaticDepositAddress(
      request: RotateStaticDepositAddressRequest
  ): Future[RotateStaticDepositAddressResponse] = {
    sparkClient.rotate_static_deposit_address(request)
  }

  def startDepositTreeCreation(
      request: StartDepositTreeCreationRequest
  ): Future[StartDepositTreeCreationResponse] = {
    sparkClient.start_deposit_tree_creation(request)
  }

  def finalizeDepositTreeCreation(
      request: FinalizeDepositTreeCreationRequest
  ): Future[FinalizeDepositTreeCreationResponse] = {
    sparkClient.finalize_deposit_tree_creation(request)
  }

  def finalizeTransferWithTransferPackage(
      request: FinalizeTransferWithTransferPackageRequest
  ): Future[FinalizeTransferResponse] = {
    sparkClient.finalize_transfer_with_transfer_package(request)
  }

  def queryPendingTransfers(
      request: TransferFilter
  ): Future[QueryTransfersResponse] = {
    sparkClient.query_pending_transfers(request)
  }

  def queryAllTransfers(
      request: TransferFilter
  ): Future[QueryTransfersResponse] = {
    sparkClient.query_all_transfers(request)
  }

  def claimTransferTweakKeys(
      request: ClaimTransferTweakKeysRequest
  ): Future[Empty] = {
    sparkClient.claim_transfer_tweak_keys(request)
  }

  def storePreimageShare(request: StorePreimageShareRequest): Future[Empty] = {
    sparkClient.store_preimage_share(request)
  }

  def storePreimageShareV2(
      request: StorePreimageShareV2Request
  ): Future[Empty] = {
    sparkClient.store_preimage_share_v2(request)
  }

  def getSigningCommitments(
      request: GetSigningCommitmentsRequest
  ): Future[GetSigningCommitmentsResponse] = {
    sparkClient.get_signing_commitments(request)
  }

  def providePreimage(
      request: ProvidePreimageRequest
  ): Future[ProvidePreimageResponse] = {
    sparkClient.provide_preimage(request)
  }

  def queryPreimage(
      request: QueryPreimageRequest
  ): Future[QueryPreimageResponse] = {
    sparkClient.query_preimage(request)
  }

  def queryHtlc(request: QueryHtlcRequest): Future[QueryHtlcResponse] = {
    sparkClient.query_htlc(request)
  }

  def renewLeaf(request: RenewLeafRequest): Future[RenewLeafResponse] = {
    sparkClient.renew_leaf(request)
  }

  def getSigningOperatorList(
      request: Empty
  ): Future[GetSigningOperatorListResponse] = {
    sparkClient.get_signing_operator_list(request)
  }

  def queryNodes(request: QueryNodesRequest): Future[QueryNodesResponse] = {
    sparkClient.query_nodes(request)
  }

  def queryBalance(
      request: QueryBalanceRequest
  ): Future[QueryBalanceResponse] = {
    sparkClient.query_balance(request)
  }

  def queryUserSignedRefunds(
      request: QueryUserSignedRefundsRequest
  ): Future[QueryUserSignedRefundsResponse] = {
    sparkClient.query_user_signed_refunds(request)
  }

  def queryUnusedDepositAddresses(
      request: QueryUnusedDepositAddressesRequest
  ): Future[QueryUnusedDepositAddressesResponse] = {
    sparkClient.query_unused_deposit_addresses(request)
  }

  def queryStaticDepositAddresses(
      request: QueryStaticDepositAddressesRequest
  ): Future[QueryStaticDepositAddressesResponse] = {
    sparkClient.query_static_deposit_addresses(request)
  }

  def subscribeToEvents(
      request: SubscribeToEventsRequest
  ): Source[SubscribeToEventsResponse, NotUsed] = {
    sparkClient.subscribe_to_events(request)
  }

  def initiateStaticDepositUtxoRefund(
      request: InitiateStaticDepositUtxoRefundRequest
  ): Future[InitiateStaticDepositUtxoRefundResponse] = {
    sparkClient.initiate_static_deposit_utxo_refund(request)
  }

  def exitSingleNodeTrees(
      request: ExitSingleNodeTreesRequest
  ): Future[ExitSingleNodeTreesResponse] = {
    sparkClient.exit_single_node_trees(request)
  }

  def cooperativeExitV2(
      request: CooperativeExitRequest
  ): Future[CooperativeExitResponse] = {
    sparkClient.cooperative_exit_v2(request)
  }

  def claimTransferSignRefundsV2(
      request: ClaimTransferSignRefundsRequest
  ): Future[ClaimTransferSignRefundsResponse] = {
    sparkClient.claim_transfer_sign_refunds_v2(request)
  }

  def finalizeNodeSignaturesV2(
      request: FinalizeNodeSignaturesRequest
  ): Future[FinalizeNodeSignaturesResponse] = {
    sparkClient.finalize_node_signatures_v2(request)
  }

  def initiatePreimageSwapV2(
      request: InitiatePreimageSwapRequest
  ): Future[InitiatePreimageSwapResponse] = {
    sparkClient.initiate_preimage_swap_v2(request)
  }

  def initiatePreimageSwapV3(
      request: InitiatePreimageSwapRequest
  ): Future[InitiatePreimageSwapResponse] = {
    sparkClient.initiate_preimage_swap_v3(request)
  }

  def startLeafSwapV2(
      request: StartTransferRequest
  ): Future[StartTransferResponse] = {
    sparkClient.start_leaf_swap_v2(request)
  }

  def startTransferV2(
      request: StartTransferRequest
  ): Future[StartTransferResponse] = {
    sparkClient.start_transfer_v2(request)
  }

  def startTransferV3(
      request: StartTransferV3Request
  ): Future[StartTransferResponse] = {
    sparkClient.start_transfer_v3(request)
  }

  def claimTransfer(
      request: ClaimTransferRequest
  ): Future[ClaimTransferResponse] = {
    sparkClient.claim_transfer(request)
  }

  def getUtxosForAddress(
      request: GetUtxosForAddressRequest
  ): Future[GetUtxosForAddressResponse] = {
    sparkClient.get_utxos_for_address(request)
  }

  def getUtxosForIdentity(
      request: GetUtxosForIdentityRequest
  ): Future[GetUtxosForIdentityResponse] = {
    sparkClient.get_utxos_for_identity(request)
  }

  def querySparkInvoices(
      request: QuerySparkInvoicesRequest
  ): Future[QuerySparkInvoicesResponse] = {
    sparkClient.query_spark_invoices(request)
  }

  def initiateSwapPrimaryTransfer(
      request: InitiateSwapPrimaryTransferRequest
  ): Future[InitiateSwapPrimaryTransferResponse] = {
    sparkClient.initiate_swap_primary_transfer(request)
  }

  def updateWalletSetting(
      request: UpdateWalletSettingRequest
  ): Future[UpdateWalletSettingResponse] = {
    sparkClient.update_wallet_setting(request)
  }

  def queryWalletSetting(
      request: QueryWalletSettingRequest
  ): Future[QueryWalletSettingResponse] = {
    sparkClient.query_wallet_setting(request)
  }

  def getChallenge(
      request: GetChallengeRequest
  ): Future[GetChallengeResponse] = {
    sparkAuthnClient.get_challenge(request)
  }

  def verifyChallenge(
      request: VerifyChallengeRequest
  ): Future[VerifyChallengeResponse] = {
    sparkAuthnClient.verify_challenge(request)
  }

  def startTransaction(
      request: StartTransactionRequest
  ): Future[StartTransactionResponse] = {
    sparkTokenClient.start_transaction(request)
  }

  def commitTransaction(
      request: CommitTransactionRequest
  ): Future[CommitTransactionResponse] = {
    sparkTokenClient.commit_transaction(request)
  }

  def queryTokenMetadata(
      request: QueryTokenMetadataRequest
  ): Future[QueryTokenMetadataResponse] = {
    sparkTokenClient.query_token_metadata(request)
  }

  def queryTokenTransactions(
      request: QueryTokenTransactionsRequest
  ): Future[QueryTokenTransactionsResponse] = {
    sparkTokenClient.query_token_transactions(request)
  }

  def queryTokenOutputs(
      request: QueryTokenOutputsRequest
  ): Future[QueryTokenOutputsResponse] = {
    sparkTokenClient.query_token_outputs(request)
  }

  def freezeTokens(
      request: FreezeTokensRequest
  ): Future[FreezeTokensResponse] = {
    sparkTokenClient.freeze_tokens(request)
  }

  def broadcastTransaction(
      request: BroadcastTransactionRequest
  ): Future[BroadcastTransactionResponse] = {
    sparkTokenClient.broadcast_transaction(request)
  }
}
