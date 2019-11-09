package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons.LogIn

class UserManager extends Actor {

  import UserManagerMessage._
  private val playerManager = context.actorOf(PlayerManager())

  override def receive: Receive = {
    case msg: LogIn => playerManager ! LogInMessage(msg, sender())
    case msg: RetrieveAllPlayers => playerManager ! msg
  }

}

object UserManager {

  def apply(): Props = Props(classOf[UserManager])

  private abstract class LobbyManager extends Actor

}

private[user_manager] object UserManagerMessage {
  case class LogInMessage(message: LogIn, user: ActorRef)
  case class RetrieveAllPlayers(sender: ActorRef)
  case class Players(players: Set[UserInfo])

  case class UserInfo(username: String, userRef: ActorRef)

}
