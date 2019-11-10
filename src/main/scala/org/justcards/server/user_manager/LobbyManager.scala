package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.justcards.server.user_manager.UserManagerMessage.{GetLobbies, UserCreateLobby, UserInfo}

private[user_manager] class LobbyManager(knowledgeEngine: ActorRef) extends Actor {

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
        if(result.isFailure) userInfo.userRef ! ErrorOccurred("The game doesn't exist!")
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
}

sealed trait Lobby {
  def id: Long
  def owner: UserInfo
  def -->(newOwner: UserInfo): Lobby
  def members: Traversable[String]
  def +(member: UserInfo): Lobby
  def -(member: UserInfo): Lobby
}

object Lobby {

  def apply(id: Long, owner: UserInfo, game: GameId): Lobby = LobbyImpl(id, owner, game)

  private[this] case class LobbyImpl(id: Long, owner: UserInfo, private val game: GameId,
                                     private val _members: Set[UserInfo] = Set()) extends Lobby {


    override def -->(newOwner: UserInfo): Lobby = LobbyImpl(id, newOwner, game, _members)

    override def members: Traversable[String] = _members map(_.username)

    override def +(member: UserInfo): Lobby = LobbyImpl(id, owner, game, _members + member)

    override def -(member: UserInfo): Lobby = LobbyImpl(id, owner, game, _members - member)
  }


}
