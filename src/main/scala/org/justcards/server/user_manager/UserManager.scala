package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons.LogIn

class UserManager extends Actor {

  import UserManagerMessage._
  private val playerManager = context.actorOf(PlayerManager())

  override def receive: Receive = {
    case msg: LogIn => playerManager ! UserLogIn(msg, sender())
    case _: Players =>
    case msg: UserManagerMessage => playerManager ! msg
  }

}

object UserManager {

  def apply(): Props = Props(classOf[UserManager])

  private abstract class LobbyManager extends Actor

}

private[user_manager] object UserManagerMessage {

  sealed trait UserManagerMessage

  case class UserLogIn(message: LogIn, user: ActorRef) extends UserManagerMessage
  case class UserLogout(username: String, user: ActorRef) extends UserManagerMessage

  case class RetrieveAllPlayers(sender: ActorRef) extends UserManagerMessage
  case class Players(players: Set[UserInfo]) extends UserManagerMessage

  case class UserInfo(username: String, userRef: ActorRef)

}
