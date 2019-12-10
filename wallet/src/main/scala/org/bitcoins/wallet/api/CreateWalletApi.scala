package org.bitcoins.wallet.api

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.crypto.MnemonicCode
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.wallet.config.WalletAppConfig

/**
  * @define initialize
  *                    Initializes the wallet, generating a wallet seed.
  *                    This seed should be displayed to the user, so they
  *                    can write it down. They should also be prompted
  *                    to confirm at least parts of the code.
  *
  *
  * @define initializeWithEnt
  *                           Initializes the with a user-provided seed,
  *                           generating a wallet seed.
  *                           This seed should be displayed to the user, so they
  *                           can write it down. They should also be prompted
  *                           to confirm at least parts of the code.
  */
trait CreateWalletApi {

  private def initializeInternal(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit executionContext: ExecutionContext,
      config: WalletAppConfig): Future[InitializeWalletResult] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits,
                          nodeApi,
                          chainQueryApi)

  /**
    * $initialize
    */
  final def initialize(nodeApi: NodeApi, chainQueryApi: ChainQueryApi)(
      implicit executionContext: ExecutionContext,
      config: WalletAppConfig): Future[InitializeWalletResult] =
    initializeInternal(nodeApi, chainQueryApi)

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(
      entropy: BitVector,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit config: WalletAppConfig,
      executionContext: ExecutionContext): Future[InitializeWalletResult]

  // todo: scaladoc
  final def initializeWithMnemonic(
      mnemonicCode: MnemonicCode,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit config: WalletAppConfig,
      executionContext: ExecutionContext): Future[InitializeWalletResult] = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(entropy, nodeApi, chainQueryApi)
  }
}
