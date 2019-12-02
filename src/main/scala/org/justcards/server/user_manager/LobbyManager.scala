package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.justcards.server.user_manager.UserManagerMessage._
import Lobby._
import org.justcards.server.Commons.UserInfo

private[user_manager] class LobbyManager(knowledgeEngine: ActorRef, lobbyDatabase: LobbyDatabase) extends Actor {

  import org.justcards.commons.AppError._

  override def receive: Receive = defaultBehaviour(lobbyDatabase)

  private def defaultBehaviour(lobbies: LobbyDatabase): Receive = {
    case GetLobbies(msg, userInfo) =>
      val result = searchLobbyWithFilters(msg.gameName, msg.ownerName)(lobbies)
      userInfo.userRef ! AvailableLobbies(result)
    case UserCreateLobby(CreateLobby(gameId), userInfo) => createLobby(lobbies)(gameId, userInfo)
    case UserJoinLobby(JoinLobby(lobbyId), userInfo) => joinUserToLobby(lobbies)(lobbyId.id, userInfo)
    case UserExitFromLobby(lobbyId, userInfo) =>
      val userRemoved = exitUserFromLobby(lobbies)(lobbyId.id, userInfo)
      sender() ! UserRemoved(userRemoved)
  }

  private def createLobby(lobbies: LobbyDatabase)(gameId: GameId, userInfo: UserInfo): Unit = {
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

  private def joinUserToLobby(lobbies: LobbyDatabase)(id: Long, userInfo: UserInfo): Unit = {
    if(!(lobbies contains id)) userInfo.userRef ! ErrorOccurred(LOBBY_NOT_EXISTING)
    else {
      val userInAnotherLobby = lobbies find (_.members contains userInfo)
      if(userInAnotherLobby isDefined) userInfo.userRef ! ErrorOccurred(USER_ALREADY_IN_A_LOBBY)
      else {
        val lobby = lobbies(id)
        if(lobby isFull) userInfo.userRef ! ErrorOccurred(LOBBY_FULL)
        else {
          val newLobby = addUserToLobby(lobby, userInfo)
          context become defaultBehaviour(lobbies + newLobby)
        }
      }
    }
  }

  private def exitUserFromLobby(lobbies: LobbyDatabase)(id: Long, userInfo: UserInfo): Boolean = {
    val lobby = lobbies.find(_.id == id)
    val userFoundInLobby = lobby.isDefined && (lobby.get.members contains userInfo)
    if(userFoundInLobby) {
      val newLobby = lobby.get - userInfo
      if(newLobby isDefined) {
        val newLobbyVal = newLobby.get
        val newMembers = newLobbyVal.members map (user => UserId(1, user.username))
        val lobbyId = LobbyId(id, newLobbyVal.owner.username, newLobbyVal.game)
        newLobbyVal.members foreach {
          _.userRef ! LobbyUpdate(lobbyId, newMembers)
        }
        context become defaultBehaviour(lobbies + newLobbyVal)
      } else
        context become defaultBehaviour(lobbies - lobby.get)
    }
    userFoundInLobby
  }

  private def addUserToLobby(lobby: Lobby, userInfo: UserInfo): Lobby = {
    val newLobby =  lobby + userInfo
    val newMembers = newLobby.members map (user => UserId(1, user.username))
    val lobbyId = LobbyId(lobby.id, lobby.owner.username, lobby.game)
    userInfo.userRef ! LobbyJoined(lobbyId, newMembers)
    newLobby.members filter (_ != userInfo) foreach {
      _.userRef ! LobbyUpdate(lobbyId, newMembers)
    }
    newLobby
  }

  private def searchLobbyWithFilters(game: String, owner: String)(lobbies: LobbyDatabase): Set[(LobbyId, Set[UserId])] = {
    var availableLobbies = lobbies.filter(!_.isFull)
    val gameToSearch = game.toLowerCase
    if(!owner.isEmpty)
      availableLobbies = availableLobbies.filter(_.owner.username == owner)
    if(!game.isEmpty)
      availableLobbies = availableLobbies.filter(_.game.name.toLowerCase == gameToSearch)
    availableLobbies.map(lobbyInfo =>
      lobbyToLobbyId(lobbyInfo) -> lobbyInfo.members.map(user => UserId(1,user.username))
    )
  }
}

private[user_manager] object LobbyManager {
  def apply(knowledgeEngine: ActorRef): Props =
    Props(classOf[LobbyManager], knowledgeEngine, LobbyDatabase.createMapLobbyDatabase())
  def apply(knowledgeEngine: ActorRef, lobbyDatabase: LobbyDatabase): Props =
    Props(classOf[LobbyManager], knowledgeEngine, lobbyDatabase)
}
