package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.justcards.server.user_manager.UserManagerMessage.{GetLobbies, UserCreateLobby, UserInfo, UserJoinLobby}

private[user_manager] class LobbyManager(knowledgeEngine: ActorRef) extends Actor {

  import LobbyManager._

  override def receive: Receive = defaultBehaviour()

  private def defaultBehaviour(lobbies: Map[Long, Lobby] = Map()): Receive = {
    case GetLobbies(sender) =>
      val availableLobbies = lobbies.map(lobbyInfo =>
        LobbyId(lobbyInfo._1) -> lobbyInfo._2.members.map(user => UserId(1,user.username))
      )
      sender ! AvailableLobbies(availableLobbies.toSet)
    case UserCreateLobby(CreateLobby(gameId), userInfo) =>
      import context.dispatcher
      implicit val timeout = Timeout(3 seconds)
      val request = knowledgeEngine ? GameExistenceRequest(gameId)
      request filter {
        case GameExistenceResponse(response) => response
      } onComplete { result =>
        if(result.isFailure) userInfo.userRef ! ErrorOccurred(GAME_NOT_EXISTING)
        else {
          val newLobby = Lobby(System.currentTimeMillis(), userInfo, gameId)
          userInfo.userRef ! LobbyCreated(LobbyId(newLobby.id))
          context become defaultBehaviour(lobbies + (newLobby.id -> newLobby))
        }
      }
    case UserJoinLobby(JoinLobby(lobbyId), userInfo) =>
      if(!(lobbies contains lobbyId.id)) userInfo.userRef ! ErrorOccurred(LOBBY_NOT_EXISTING)
      else {
        val userInAnotherLobby = lobbies find (_._2.members contains userInfo)
        if(userInAnotherLobby isDefined) userInfo.userRef ! ErrorOccurred(ALREADY_IN_A_LOBBY)
        else {
          val newLobby = lobbies(lobbyId.id) + userInfo
          val newMembers = newLobby.members map (user => UserId(1, user.username))
          userInfo.userRef ! LobbyJoined(lobbyId, newMembers)
          newLobby.members filter (_ != userInfo) foreach {
            _.userRef ! LobbyUpdate(lobbyId, newMembers)
          }
          context become defaultBehaviour(lobbies + (newLobby.id -> newLobby))
        }
      }

  }


}

private[user_manager] object LobbyManager {
  def apply(knowledgeEngine: ActorRef): Props = Props(classOf[LobbyManager], knowledgeEngine)

  private val GAME_NOT_EXISTING = "The game doesn't exist!"
  private val LOBBY_NOT_EXISTING = "The lobby doesn't exist!"
  private val ALREADY_IN_A_LOBBY = "You're already a member of another lobby!"
}

/**
  * Class that represents all the information of a lobby
  */
sealed trait Lobby {
  /**
    * Getter
    * @return the lobby is
    */
  def id: Long

  /**
    * Getter
    * @return the lobby owner
    */
  def owner: UserInfo

  /**
    * Add a new owner to the lobby
    * @param newOwner the new owner of the lobby
    * @return a new lobby with the new lobby
    */
  def -->(newOwner: UserInfo): Lobby

  /**
    * Getter.
    * @return the lobby members
    */
  def members: Set[UserInfo]

  /**
    * Add a member to the lobby
    * @param member the new member
    * @return a new lobby that contains the new member
    */
  def +(member: UserInfo): Lobby

  /**
    * Remove a member from the lobby
    * @param member the member to remove
    * @return a new lobby where the member is not present
    */
  def -(member: UserInfo): Lobby
}

object Lobby {

  /**
    * Creates a new lobby
    * @param id the lobby id
    * @param owner the owner of the lobby
    * @param game the game
    * @return a new lobby
    */
  def apply(id: Long, owner: UserInfo, game: GameId): Lobby = LobbyImpl(id, owner, game, Set(owner))

  private[this] case class LobbyImpl(id: Long, owner: UserInfo, private val game: GameId,
                                     members: Set[UserInfo]) extends Lobby {


    override def -->(newOwner: UserInfo): Lobby = LobbyImpl(id, newOwner, game, members)

    override def +(member: UserInfo): Lobby = LobbyImpl(id, owner, game, members + member)

    override def -(member: UserInfo): Lobby = LobbyImpl(id, owner, game, members - member)
  }


}
