package org.bitcoins.core.util

import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by chris on 3/11/16.
  */
trait BitcoinSLogger {
  def logger: Logger = LoggerFactory.getLogger(getClass)
}

object BitcoinSLogger extends BitcoinSLogger
