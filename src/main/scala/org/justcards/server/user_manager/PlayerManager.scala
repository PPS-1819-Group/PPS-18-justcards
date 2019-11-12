package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons.{ErrorOccurred, Logged}
import org.justcards.server.user_manager.UserManagerMessage._

private[user_manager] class PlayerManager extends Actor {

  import PlayerManager._

  override def receive: Receive = defaultBehaviour()

  private def defaultBehaviour(users: Set[(String, ActorRef)] = Set()): Receive = {
    case UserLogIn(message, user) =>
      val loggedUser = users find(userData =>
        userData._2 == user || userData._1 == message.username
      ) map(_._1)
      if (loggedUser.isDefined && loggedUser.get == message.username)
        user ! ErrorOccurred(getUsernameError(message.username))
      else if (loggedUser.isDefined)
        user ! ErrorOccurred(ALREADY_LOGGED)
      else {
        user ! Logged(message.username)
        val updatedUsers = users + (message.username -> user)
        context become defaultBehaviour(updatedUsers)
      }
    case UserLogout(username, user) =>
      if (users contains (username -> user))
        context become defaultBehaviour(users - (username -> user))
    case RetrieveAllPlayers(msgSender) =>
      val usersSet: Set[UserInfo] = users map(data => UserInfo(data._1, data._2))
      msgSender ! Players(usersSet)
    case PlayerLogged(user) =>
      val userRes = users.find(_._2 == user).map(_._1)
      if(userRes.isDefined) sender() ! PlayerLoggedResult(found = true, userRes.get)
      else sender() ! PlayerLoggedResult(found = false, "")
  }
}

private[user_manager] object PlayerManager {
  def apply(): Props = Props(classOf[PlayerManager])

  private val getUsernameError: String => String = username => username + " is already present!"
  private val ALREADY_LOGGED = "You're already logged with another username!"
}
