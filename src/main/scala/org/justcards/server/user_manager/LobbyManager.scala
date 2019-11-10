package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.justcards.server.user_manager.UserManagerMessage.{GetLobbies, UserCreateLobby, UserInfo}

private[user_manager] class LobbyManager(knowledgeEngine: ActorRef) extends Actor {

  import LobbyManager._

  override def receive: Receive = defaultBehaviour()

  private def defaultBehaviour(lobbies: Map[Long, Lobby] = Map()): Receive = {
    case GetLobbies(sender) =>
      sender ! AvailableLobbies(lobbies.keySet.map(LobbyId))
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

  }


}

private[user_manager] object LobbyManager {
  def apply(knowledgeEngine: ActorRef): Props = Props(classOf[LobbyManager], knowledgeEngine)

  private val GAME_NOT_EXISTING = "The game doesn't exist!"
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
  def members: Traversable[String]

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
  def apply(id: Long, owner: UserInfo, game: GameId): Lobby = LobbyImpl(id, owner, game)

  private[this] case class LobbyImpl(id: Long, owner: UserInfo, private val game: GameId,
                                     private val _members: Set[UserInfo] = Set()) extends Lobby {


    override def -->(newOwner: UserInfo): Lobby = LobbyImpl(id, newOwner, game, _members)

    override def members: Traversable[String] = _members map(_.username)

    override def +(member: UserInfo): Lobby = LobbyImpl(id, owner, game, _members + member)

    override def -(member: UserInfo): Lobby = LobbyImpl(id, owner, game, _members - member)
  }


}
