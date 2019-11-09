package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons.{ErrorOccurred, Logged}
import org.justcards.server.user_manager.UserManagerMessage._

private[user_manager] class PlayerManager extends Actor {

  override def receive: Receive = defaultBehaviour()

  private def defaultBehaviour(users: Map[String, ActorRef] = Map()): Receive = {
    case UserLogIn(message, user) =>
      if (users contains message.username)
        user ! ErrorOccurred(message.username + " is already present!")
      else {
        user ! Logged(message.username)
        val updatedUsers = users + (message.username -> user)
        context become defaultBehaviour(updatedUsers)
      }
    case UserLogout(username, user) =>
      if (users exists ( _ == (username, user)))
        context become defaultBehaviour(users - username)
    case RetrieveAllPlayers(msgSender) =>
      val usersSet: Set[UserInfo] = users.map(data => UserInfo(data._1, data._2)).toSet
      msgSender ! Players(usersSet)
  }
}

private[user_manager] object PlayerManager {
  def apply(): Props = Props(classOf[PlayerManager])
}
