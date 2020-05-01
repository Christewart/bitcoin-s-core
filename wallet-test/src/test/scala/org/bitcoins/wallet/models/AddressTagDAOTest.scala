package org.bitcoins.wallet.models

import java.sql.SQLException

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.wallet.utxo.StorageLocationTag.HotStorage
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  AddressTagFactory,
  ExternalAddressTag,
  ExternalAddressTagName,
  ExternalAddressTagType,
  ExternalAddressTagWrapper,
  StorageLocationTag,
  UnknownAddressTagName,
  UnknownAddressTagType
}
import org.bitcoins.testkit.fixtures.WalletDAOFixture
import org.bitcoins.testkit.util.TestUtil
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletTestUtil}
import org.scalatest.Assertion

import scala.concurrent.Future

class AddressTagDAOTest extends BitcoinSWalletTest with WalletDAOFixture {

  behavior of "AddressTagDAO"

  val exampleTag: ExternalAddressTagWrapper =
    ExternalAddressTagWrapper(UnknownAddressTagName("Example"),
                              UnknownAddressTagType("ExampleTagType"))

  def testInsertionFailure(
      daos: FixtureParam,
      tag: AddressTag): Future[Assertion] = {
    val tagDAO = daos.addressTagDAO
    val addr = TestUtil.testBitcoinAddress.get
    val tagDb = AddressTagDb(addr, tag.tagName, tag.tagType)
    val readF = tagDAO.create(tagDb)

    recoverToSucceededIf[SQLException](readF)
  }

  def testInsertion(daos: FixtureParam, tag: AddressTag): Future[Assertion] = {
    val accountDAO = daos.accountDAO
    val addressDAO = daos.addressDAO
    val addressTagDAO = daos.addressTagDAO
    for {
      createdAccount <- {
        val account = WalletTestUtil.firstAccountDb
        accountDAO.create(account)
      }
      createdAddress <- {
        val addressDb = WalletTestUtil.getAddressDb(createdAccount)
        addressDAO.create(addressDb)
      }
      createdAddressTag <- {
        val tagDb =
          AddressTagDb(createdAddress.address, tag)
        addressTagDAO.create(tagDb)
      }
      readAddressTagOpt <- addressTagDAO.read(createdAddressTag.address)
    } yield {
      assert(readAddressTagOpt.isDefined)
      val readAddressTag = readAddressTagOpt.get

      assert(readAddressTag.address == createdAddress.address)
      assert(readAddressTag.addressTag == tag)
    }
  }

  it should "fail to insert and read an unknown address tag into the database without a corresponding address" in {
    daos =>
      testInsertionFailure(daos, exampleTag)
  }

  it should "fail to insert and read an internal address tag into the database without a corresponding address" in {
    daos =>
      testInsertionFailure(daos, HotStorage)
  }

  it should "insert and read an unknown address tag into the database" in {
    daos =>
      testInsertion(daos, exampleTag)
  }

  it should "insert and read an internal address tag into the database" in {
    daos =>
      testInsertion(daos, HotStorage)
  }

  // EXAMPLE BEGIN

  sealed trait ExternalExampleTagName extends ExternalAddressTagName

  case object ExternalExampleTagType extends ExternalAddressTagType {
    override lazy val typeName: String = "Example"
  }

  sealed trait ExternalExampleTag extends ExternalAddressTag {
    override def tagName: ExternalExampleTagName
    override val tagType: ExternalAddressTagType = ExternalExampleTagType
  }

  object ExternalExampleTag extends AddressTagFactory[ExternalAddressTag] {

    override val tagType: ExternalExampleTagType.type = ExternalExampleTagType

    override val tagNames = Vector(ExampleAName, ExampleBName, ExampleCName)

    override val all = Vector(ExampleA, ExampleB, ExampleC)

    case object ExampleAName extends ExternalExampleTagName {
      override val name: String = "A"
    }

    case object ExampleBName extends ExternalExampleTagName {
      override val name: String = "B"
    }

    case object ExampleCName extends ExternalExampleTagName {
      override val name: String = "C"
    }

    case object ExampleA extends ExternalExampleTag {
      override val tagName: ExternalExampleTagName = ExampleAName
    }

    case object ExampleB extends ExternalExampleTag {
      override val tagName: ExternalExampleTagName = ExampleBName
    }

    case object ExampleC extends ExternalExampleTag {
      override val tagName: ExternalExampleTagName = ExampleCName
    }
  }

  case class Example(dao: AddressTagDAO) {

    def writeAllToDb(address: BitcoinAddress): Future[Vector[AddressTagDb]] = {

      val addressDbA = AddressTagDb(address, ExternalExampleTag.ExampleA)
      val addressDbB = AddressTagDb(address, ExternalExampleTag.ExampleB)
      val addressDbC = AddressTagDb(address, ExternalExampleTag.ExampleC)

      dao.createAll(Vector(addressDbA, addressDbB, addressDbC))
    }

    def readAllFromDb(address: BitcoinAddress): Future[Vector[AddressTag]] = {
      dao.findByAddress(address).map { tags =>
        tags.map(_.addressTag).map {
          case tag: StorageLocationTag => tag
          case tag: ExternalAddressTagWrapper =>
            tag.toExternal(ExternalExampleTag.fromString)
        }
      }
    }
  }

  // EXAMPLE END

  it should "work for Nadav's example" in { daos =>
    import WalletTestUtil._

    val dao = daos.addressTagDAO
    val example = Example(dao)

    val addressF = for {
      account <- daos.accountDAO.create(firstAccountDb)
      addressDb <- daos.addressDAO.create(getAddressDb(account))
    } yield addressDb.address

    for {
      address <- addressF
      _ <- example.writeAllToDb(address)
      addressTags <- example.readAllFromDb(address)
    } yield {
      println(addressTags)
      println(ExternalExampleTag.all)
      assert(addressTags == ExternalExampleTag.all)
    }
  }

  it must "create a wrapped tag from an external tag" in {
    val wrapped = ExternalAddressTagWrapper(ExternalExampleTag.ExampleA)
  }
}
