package org.justcards.server.session_manager

import akka.actor.{Actor, Props}
import org.justcards.commons.{ChooseBriscola, OutOfLobby, TimeoutExceeded, Turn}

/**
 * Class that represents a user that has logged out.
 */
class FakeUser extends Actor {

  override def receive: Receive = {
    case ChooseBriscola => sender ! TimeoutExceeded()
    case Turn => sender ! TimeoutExceeded()
    case OutOfLobby => context stop self
    case _ =>
  }
}

object FakeUser {
  def apply(): Props = {
    Props(classOf[FakeUser])
  }
}