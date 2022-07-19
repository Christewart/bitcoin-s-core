package org.bitcoins.server.util

import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.node.NodeCallbacks

case class DLCWalletWithCallbacks(
    wallet: DLCWallet,
    nodeCallbacks: NodeCallbacks)
