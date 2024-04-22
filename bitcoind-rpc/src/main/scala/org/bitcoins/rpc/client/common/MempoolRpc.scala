package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.BitcoindException
import play.api.libs.json.{JsBoolean, JsString, Json}

import scala.concurrent.Future

/** This trait defines RPC calls related to the mempool of a Bitcoin Core node.
  * The mempool contains all unconfirmed transactions.
  */
trait MempoolRpc { self: Client =>

  def getMemPoolAncestors(
      txid: DoubleSha256DigestBE
  ): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(false))
    )
  }

  def getMemPoolAncestors(
      txid: DoubleSha256Digest
  ): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolAncestors(txid.flip)
  }

  def getMemPoolAncestorsVerbose(
      txid: DoubleSha256DigestBE
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV23]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(true))
    )
  }

  def getMemPoolAncestorsVerbose(
      txid: DoubleSha256Digest
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolAncestorsVerbose(txid.flip)
  }

  def getMemPoolDescendants(
      txid: DoubleSha256DigestBE
  ): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(false))
    )
  }

  def getMemPoolDescendants(
      txid: DoubleSha256Digest
  ): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolDescendants(txid.flip)
  }

  def getMemPoolDescendantsVerbose(
      txid: DoubleSha256DigestBE
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV23]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(true))
    )
  }

  def getMemPoolDescendantsVerbose(
      txid: DoubleSha256Digest
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolDescendantsVerbose(txid.flip)
  }

  def getMemPoolEntry(
      txid: DoubleSha256DigestBE
  ): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResultPostV23](
      "getmempoolentry",
      List(JsString(txid.hex))
    )
  }

  def getMemPoolEntry(
      txid: DoubleSha256Digest
  ): Future[GetMemPoolEntryResult] = {
    getMemPoolEntry(txid.flip)
  }

  def getMemPoolEntryOpt(
      txid: DoubleSha256Digest
  ): Future[Option[GetMemPoolEntryResult]] = {
    getMemPoolEntryOpt(txid.flip)
  }

  def getMemPoolEntryOpt(
      txid: DoubleSha256DigestBE
  ): Future[Option[GetMemPoolEntryResult]] = {
    getMemPoolEntry(txid)
      .map(Some(_))
      .recover { case _: BitcoindException.InvalidAddressOrKey =>
        None
      }
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getRawMemPool: Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getrawmempool",
      List(JsBoolean(false))
    )
  }

  def getRawMemPoolWithTransactions
      : Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV23]](
      "getrawmempool",
      List(JsBoolean(true))
    )
  }

  def saveMemPool(): Future[Unit] = {
    bitcoindCall[Unit]("savemempool")
  }

  def testMempoolAccept(
      transaction: Vector[Transaction],
      maxFeeRate: Double = 0.10
  ): Future[Vector[TestMempoolAcceptResultPostV22]] = {
    bitcoindCall[Vector[TestMempoolAcceptResultPostV22]](
      "testmempoolaccept",
      List(Json.toJson(transaction), Json.toJson(maxFeeRate))
    )
  }
}
