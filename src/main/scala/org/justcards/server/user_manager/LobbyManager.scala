package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.justcards.server.user_manager.UserManagerMessage._
import Lobby._

private[user_manager] abstract class LobbyManager(knowledgeEngine: ActorRef, lobbyDatabase: LobbyDatabase) extends Actor {

  import org.justcards.commons.AppError._

  override def receive: Receive = defaultBehaviour(lobbyDatabase)

  private def defaultBehaviour(lobbies: LobbyDatabase): Receive = {
    case GetLobbies(sender) =>
      val availableLobbies: Set[(LobbyId, Set[UserId])] = lobbies.filter(!_.isFull).map(lobbyInfo =>
        lobbyToLobbyId(lobbyInfo) -> lobbyInfo.members.map(user => UserId(1,user.username))
      )
      sender ! AvailableLobbies(availableLobbies)
    case UserCreateLobby(CreateLobby(gameId), userInfo) => createLobby(lobbies)(gameId, userInfo)
    case UserJoinLobby(JoinLobby(lobbyId), userInfo) => joinUserToLobby(lobbies)(lobbyId, userInfo)
    case UserExitFromLobby(lobbyId, userInfo) => exitUserFromLobby(lobbies)(lobbyId, userInfo)
  }

  private def createLobby(lobbies: LobbyDatabase)(gameId: GameId, userInfo: UserManagerMessage.UserInfo): Unit = {
    import context.dispatcher
    implicit val timeout = Timeout(3 seconds)
    val request = knowledgeEngine ? GameExistenceRequest(gameId)
    request filter {
      case GameExistenceResponse(response) => response
    } onComplete { result =>
      if(result.isFailure) userInfo.userRef ! ErrorOccurred(GAME_NOT_EXISTING)
      else {
        val newLobby = Lobby(System.currentTimeMillis(), userInfo, gameId)
        userInfo.userRef ! LobbyCreated(newLobby)
        context become defaultBehaviour(lobbies + newLobby)
      }
    }
  }

  private def joinUserToLobby(lobbies: LobbyDatabase)(lobbyId: LobbyId, userInfo: UserInfo): Unit = {
    if(!(lobbies contains lobbyId.id)) userInfo.userRef ! ErrorOccurred(LOBBY_NOT_EXISTING)
    else {
      val userInAnotherLobby = lobbies find (_.members contains userInfo)
      if(userInAnotherLobby isDefined) userInfo.userRef ! ErrorOccurred(USER_ALREADY_IN_A_LOBBY)
      else {
        val lobby = lobbies(lobbyId.id)
        if(lobby isFull) userInfo.userRef ! ErrorOccurred(LOBBY_FULL)
        else {
          val newLobby = addUserToLobby(lobby, lobbyId, userInfo)
          context become defaultBehaviour(lobbies + newLobby)
        }
      }
    }
  }

  private def exitUserFromLobby(lobbies: LobbyDatabase)(lobbyId: LobbyId, userInfo: UserInfo): Unit = {
    val lobby = lobbies.find(_.id == lobbyId.id)
    if(lobby.isDefined && (lobby.get.members contains userInfo) ) {
      val newLobby = lobby.get - userInfo
      if(newLobby isDefined) {
        val newLobbyVal = newLobby.get
        val newMembers = newLobbyVal.members map (user => UserId(1, user.username))
        newLobbyVal.members foreach {
          _.userRef ! LobbyUpdate(lobbyId, newMembers)
        }
        context become defaultBehaviour(lobbies + newLobbyVal)
      } else
        context become defaultBehaviour(lobbies - lobby.get)
    }
  }

  private def addUserToLobby(lobby: Lobby, lobbyId: LobbyId, userInfo: UserInfo): Lobby = {
    val newLobby =  lobby + userInfo
    val newMembers = newLobby.members map (user => UserId(1, user.username))
    userInfo.userRef ! LobbyJoined(lobbyId, newMembers)
    newLobby.members filter (_ != userInfo) foreach {
      _.userRef ! LobbyUpdate(lobbyId, newMembers)
    }
    newLobby
  }


}

private[user_manager] object LobbyManager {
  def apply(knowledgeEngine: ActorRef): Props = Props(classOf[LobbyManagerWithMap], knowledgeEngine)

  private[this] class LobbyManagerWithMap(knowledgeEngine: ActorRef) extends LobbyManager(
    knowledgeEngine,
    LobbyDatabase.createMapLobbyDatabase()
  )
}
