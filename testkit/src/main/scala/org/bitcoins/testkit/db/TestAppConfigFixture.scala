package org.bitcoins.testkit.db

import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.scalatest.{BeforeAndAfterAll, FutureOutcome}
import org.scalatest.flatspec.FixtureAsyncFlatSpec

import java.nio.file.Files
import scala.concurrent.Future

trait TestAppConfigFixture
    extends FixtureAsyncFlatSpec
    with BeforeAndAfterAll
    with BitcoinSFixture
    with PostgresTestDatabase {

  override type FixtureParam = TestAppConfig

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withTestAppConfig(test)
  }

  def withTestAppConfig(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture(() => getFreshTestConfig(), destroyTestConfig)(test)
  }

  def getFreshTestConfig(): Future[TestAppConfig] = {
    val configOverride = BitcoinSTestAppConfig.configWithEmbeddedDb(
      Some(ProjectType.Test),
      postgresOpt = postgresOpt
    )
    val config =
      TestAppConfig(BitcoinSTestAppConfig.tmpDir(), Vector(configOverride))

    config.start().map { _ =>
      val _ = config.migrate()
      config
    }
  }

  def destroyTestConfig(testConfig: TestAppConfig): Future[Unit] = {
    for {
      _ <- testConfig.stop()
      _ = testConfig.driver match {
        case SQLite =>
          Files.deleteIfExists(testConfig.dbPath.resolve(testConfig.dbName))
          Files.deleteIfExists(
            testConfig.dbPath.resolve(testConfig.dbName + "-wal"))
          Files.deleteIfExists(
            testConfig.dbPath.resolve(testConfig.dbName + "-shm"))
        case PostgreSQL =>
          testConfig.clean()
      }
    } yield ()
  }
}
