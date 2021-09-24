package org.bitcoins.wallet.models

import org.bitcoins.core.api.wallet.db.{TransactionDb, TransactionDbHelper}
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.wallet.WalletTestUtil

class TransactionDAOTest extends WalletDAOFixture {

  val txDb: TransactionDb =
    TransactionDbHelper.fromTransaction(WalletTestUtil.sampleTransaction, None)

  it should "insert and read an transaction into the database" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.read(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txIdBE" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txIdBE)
    } yield assert(found.contains(created))
  }

  it must "find a transaction by txId" in { daos =>
    val txDAO = daos.transactionDAO

    for {
      created <- txDAO.create(txDb)
      found <- txDAO.findByTxId(txDb.txId)
    } yield assert(found.contains(created))
  }

  it must "write and read 3f929128e70d4186a194a35c5062263938526015e1ead85a453bd5cf71b7b9b9 for issue 3686" in {
    daos =>
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/3686
      val txDAO = daos.transactionDAO
      val hex =
        "02000000" +
          "0001" +
          "016baefa7c71b18e8f46100d56ef88c6e1fd8920c4fcda08417c5644db58de654c0100000000feffffff01400d030000000000160014e49eebc00d906a600d0843edb91e397fa3027ec4040047304402203e259c359bee8d8fe0572149ebc027173784d3b11b2f35973255b56ceb19dc1b022045dd4a9971398ddc139d5281d26402d4724b277ad6223f09cdaf48dd389d0f9b01473044022054658972cbddbd2c198b72d6f24e348ed3ab3b93e56b161abd6ede9add5c72e2022060980a31f267ce404b8398ac85f3d88708b5ff84401ff349b9986e34165ce6ec0147522102d4f3a2163c77260eca590c2c91a26365f6e890ba6d73804708ae0bb1b4faf52f2102f20e68c812235817712b2246e91e1b7a27009bac2ebb9caf459a77003b10170a52ae00000000"
      val tx = Transaction.fromHex(hex)
      assert(tx.isInstanceOf[WitnessTransaction])
      val txDb =
        TransactionDbHelper.fromTransaction(tx = tx, blockHashOpt = None)

      for {
        created <- txDAO.create(txDb)
        foundOpt <- txDAO.read(created.txIdBE)
      } yield {
        assert(foundOpt.isDefined)
        assert(foundOpt.get.transaction.isInstanceOf[WitnessTransaction])
        assert(foundOpt.get.transaction == tx)
      }
  }
}
