package org.bitcoins.core.protocol.ln.channel

/** copied from eclair [[https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/scala/fr/acinq/eclair/channel/ChannelTypes.scala]] */
sealed trait ChannelState

object ChannelState {
  case object WAIT_FOR_INIT_INTERNAL extends ChannelState
  case object WAIT_FOR_OPEN_CHANNEL extends ChannelState
  case object WAIT_FOR_ACCEPT_CHANNEL extends ChannelState
  case object WAIT_FOR_FUNDING_INTERNAL extends ChannelState
  case object WAIT_FOR_FUNDING_CREATED extends ChannelState
  case object WAIT_FOR_FUNDING_SIGNED extends ChannelState
  case object WAIT_FOR_FUNDING_CONFIRMED extends ChannelState
  case object WAIT_FOR_FUNDING_LOCKED extends ChannelState
  case object NORMAL extends ChannelState
  case object SHUTDOWN extends ChannelState
  case object NEGOTIATING extends ChannelState
  case object CLOSING extends ChannelState
  case object CLOSED extends ChannelState
  case object OFFLINE extends ChannelState
  case object SYNCING extends ChannelState
  case object WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT extends ChannelState
  case object ERR_FUNDING_LOST extends ChannelState
  case object ERR_FUNDING_TIMEOUT extends ChannelState
  case object ERR_INFORMATION_LEAK extends ChannelState

  private val all = List(
    WAIT_FOR_INIT_INTERNAL, WAIT_FOR_OPEN_CHANNEL, WAIT_FOR_ACCEPT_CHANNEL,
    WAIT_FOR_FUNDING_INTERNAL, WAIT_FOR_FUNDING_CREATED, WAIT_FOR_FUNDING_SIGNED,
    WAIT_FOR_FUNDING_CONFIRMED, WAIT_FOR_FUNDING_LOCKED, NORMAL,
    SHUTDOWN, NEGOTIATING, CLOSING,
    CLOSED, OFFLINE, SYNCING,
    WAIT_FOR_REMOTE_PUBLISH_FUTURE_COMMITMENT, ERR_FUNDING_LOST, ERR_FUNDING_TIMEOUT,
    ERR_INFORMATION_LEAK)

  def fromString(str: String): Option[ChannelState] = {
    all.find(_.toString == str)
  }
}

