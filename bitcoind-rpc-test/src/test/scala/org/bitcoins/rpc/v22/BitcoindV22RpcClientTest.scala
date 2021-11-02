package org.bitcoins.rpc.v22

import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.SetBanCommand
import org.bitcoins.core.number.UInt32
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.testkit.rpc.BitcoindFixturesCachedPairV22

import java.net.URI

class BitcoindV22RpcClientTest extends BitcoindFixturesCachedPairV22 {

  behavior of "BitcoindV22RpcClient"

  it should "be able to start a V22 bitcoind instance" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      for {
        v <- client.version
      } yield assert(v == BitcoindVersion.V22)
  }

  it should "be able to get peer info" in { nodePair: FixtureParam =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2
    for {
      infoList <- freshClient.getPeerInfo
    } yield {
      assert(infoList.length >= 0)
      val info = infoList.head
      assert(info.addnode)
      assert(info.networkInfo.addr == otherFreshClient.getDaemon.uri)
    }
  }

  it should "be able to ban and clear the ban of a subnet" in {
    nodePair: FixtureParam =>
      val client = nodePair.node1
      val loopBack = URI.create("http://127.0.0.1")
      for {
        _ <- client.setBan(loopBack, SetBanCommand.Add)
        list <- client.listBanned
        _ <- client.setBan(loopBack, SetBanCommand.Remove)
        newList <- client.listBanned
      } yield {
        assert(list.length == 1)
        assert(list.head.address.getAuthority == loopBack.getAuthority)
        assert(list.head.banned_until - list.head.ban_created == UInt32(86400))
        assert(newList.isEmpty)
      }
  }

}
