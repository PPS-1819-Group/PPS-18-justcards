package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._

/**
  * Actor that manages all the users in the system
  * @param knowledgeEngine the system knowledgeEngine
  */
class UserManager(knowledgeEngine: ActorRef) extends Actor {

  import UserManagerMessage._
  import UserManager._
  import context.dispatcher
  private implicit val timeout = Timeout(3 seconds)
  private val playerManager = context.actorOf(PlayerManager())
  private val lobbyManager = context.actorOf(LobbyManager(knowledgeEngine))

  override def receive: Receive = {
    case msg: LogIn => playerManager ! UserLogIn(msg, sender())
    case msg: LogOut => playerManager ! UserLogout(msg.username, sender())
    case _: RetrieveAvailableLobbies =>
      val user = sender()
      checkLogInAnd(user) (
        _ => lobbyManager ! GetLobbies(user),
        () => user ! ErrorOccurred(NOT_LOGGED)
      )
    case msg: CreateLobby =>
      val user = sender()
      checkLogInAnd(user) (
        username => lobbyManager ! UserCreateLobby(msg, UserInfo(username, user)),
        () => user ! ErrorOccurred(NOT_LOGGED)
      )
    case msg: JoinLobby =>
      val user = sender()
      checkLogInAnd(user) (
        username => lobbyManager ! UserJoinLobby(msg, UserInfo(username, user)),
        () => user ! ErrorOccurred(NOT_LOGGED)
      )
    case _: Players =>
    case msg: UserManagerMessage => playerManager ! msg
  }

  private def checkLogInAnd(user: ActorRef)(onComplete: String => Unit, onError: () => Unit) : Unit = {
    val request = playerManager ? PlayerLogged(user)
    request collect {
      case PlayerLoggedResult(true, username) => username
    } onComplete { result =>
      if(result.isSuccess) onComplete(result.get)
      else onError()
    }
  }

}

object UserManager {
  def apply(knowledgeEngine: ActorRef): Props = Props(classOf[UserManager], knowledgeEngine)

  private val NOT_LOGGED = "You're not logged"
}

private[user_manager] object UserManagerMessage {

  sealed trait UserManagerMessage

  case class UserLogIn(message: LogIn, user: ActorRef) extends UserManagerMessage
  case class UserLogout(username: String, user: ActorRef) extends UserManagerMessage

  case class RetrieveAllPlayers(sender: ActorRef) extends UserManagerMessage
  case class Players(players: Set[UserInfo]) extends UserManagerMessage
  case class PlayerLogged(user: ActorRef) extends UserManagerMessage
  case class PlayerLoggedResult(found: Boolean, username: String) extends UserManagerMessage

  case class GetLobbies(sender: ActorRef) extends UserManagerMessage
  case class UserCreateLobby(message: CreateLobby, user: UserInfo) extends UserManagerMessage
  case class UserJoinLobby(message: JoinLobby, user: UserInfo) extends UserManagerMessage

  case class UserInfo(username: String, userRef: ActorRef)

}
