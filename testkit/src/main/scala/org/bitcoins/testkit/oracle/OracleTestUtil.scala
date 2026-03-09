package org.bitcoins.testkit.oracle

import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.testkit.util.FileUtil

import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}

object OracleTestUtil {

  def destroyDLCOracleAppConfig(
      config: DLCOracleAppConfig
  )(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- config.stop()
      _ = config.driver match {
        case SQLite =>
          Files.deleteIfExists(config.dbPath.resolve(config.dbName))
          Files.deleteIfExists(config.dbPath.resolve(config.dbName + "-wal"))
          Files.deleteIfExists(config.dbPath.resolve(config.dbName + "-shm"))
        case PostgreSQL =>
          config.clean()
      }
      _ = FileUtil.deleteTmpDir(config.baseDatadir)
    } yield ()
  }
}
