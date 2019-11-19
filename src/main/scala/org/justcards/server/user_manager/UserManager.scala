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
class UserManager(private val knowledgeEngine: ActorRef) extends Actor {

  import UserManagerMessage._
  import org.justcards.commons.AppError._
  import context.dispatcher
  private implicit val timeout = Timeout(3 seconds)
  private val playerManager = context.actorOf(PlayerManager())
  private val lobbyManager = context.actorOf(LobbyManager(knowledgeEngine))

  override def receive: Receive = {
    case msg: LogIn => playerManager ! UserLogIn(msg, sender())
    case LogOut(username) => playerManager ! UserLogout(username, sender())
    case LogOutAndExitFromLobby(username, lobbyId) =>
      playerManager ! UserLogout(username, sender())
      lobbyManager ! UserExitFromLobby(lobbyId, UserInfo(username, sender()))
    case _: RetrieveAvailableGames =>
      val user = sender()
      (knowledgeEngine ? RetrieveAvailableGames()) onComplete { result =>
        if(result.isSuccess) user ! result.get
        else user ! ErrorOccurred(MESSAGE_SENDING_FAILED)
      }
    case _: RetrieveAvailableLobbies =>
      val user = sender()
      checkLogInAnd(user) { _ => 
        lobbyManager ! GetLobbies(user)
      }
    case msg: CreateLobby =>
      val user = sender()
      checkLogInAnd(user) { username =>
        lobbyManager ! UserCreateLobby(msg, UserInfo(username, user))
      }
    case msg: JoinLobby =>
      val user = sender()
      checkLogInAnd(user) { username =>
        lobbyManager ! UserJoinLobby(msg, UserInfo(username, user))
      }
    case _: Players =>
    case msg: UserManagerMessage => playerManager ! msg
  }

  private def checkLogInAnd(user: ActorRef)(onComplete: String => Unit) : Unit = {
    val request = playerManager ? PlayerLogged(user)
    request collect {
      case PlayerLoggedResult(true, username) => username
    } onComplete { result =>
      if(result.isSuccess) onComplete(result.get)
      else user ! ErrorOccurred(USER_NOT_LOGGED)
    }
  }

}

object UserManager {
  def apply(knowledgeEngine: ActorRef): Props = Props(classOf[UserManager], knowledgeEngine)
}

private[user_manager] object UserManagerMessage {

  sealed trait UserManagerMessage

  case class UserLogIn(message: LogIn, user: ActorRef) extends UserManagerMessage
  case class UserLogout(username: String, user: ActorRef) extends UserManagerMessage
  case class LogOutAndExitFromLobby(username: String, lobbyId: LobbyId) extends  UserManagerMessage

  case class RetrieveAllPlayers(sender: ActorRef) extends UserManagerMessage
  case class Players(players: Set[UserInfo]) extends UserManagerMessage
  case class PlayerLogged(user: ActorRef) extends UserManagerMessage
  case class PlayerLoggedResult(found: Boolean, username: String) extends UserManagerMessage

  case class GetLobbies(sender: ActorRef) extends UserManagerMessage
  case class UserCreateLobby(message: CreateLobby, user: UserInfo) extends UserManagerMessage
  case class UserJoinLobby(message: JoinLobby, user: UserInfo) extends UserManagerMessage
  case class UserExitFromLobby(lobbyId: LobbyId, userInfo: UserInfo) extends UserManagerMessage

  case class UserInfo(username: String, userRef: ActorRef)

}
