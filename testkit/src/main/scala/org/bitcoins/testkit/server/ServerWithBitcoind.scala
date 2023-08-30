package org.bitcoins.testkit.server

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoinSServerMain
import org.bitcoins.wallet.WalletHolder

case class ServerWithBitcoind(
    bitcoind: BitcoindRpcClient,
    server: BitcoinSServerMain,
    walletHolder: WalletHolder)
