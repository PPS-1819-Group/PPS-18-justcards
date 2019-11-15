package org.justcards.server.knowledge_engine

import akka.actor.{Actor, ActorRef, Props}
import org.justcards.commons._
import org.justcards.commons.GameId

class KnowledgeEngine(private val gameKnowledge: GameKnowledge) extends Actor {

  import KnowledgeEngine._

  override def receive: Receive = {
    case GameExistenceRequest(game) => sender() ! GameExistenceResponse(gameKnowledge.gameExists(game))
    case RetrieveAvailableGames(_) => sender() ! AvailableGames(gameKnowledge.availableGames)
  }

}

object KnowledgeEngine {
  def apply(gameKnowledge: GameKnowledge): Props = Props(classOf[KnowledgeEngine], gameKnowledge)

  case class GameExistenceRequest(gameId: GameId)
  case class GameExistenceResponse(existence: Boolean)
}

trait GameKnowledge {

  def availableGames: Set[GameId]
  def gameExists(game: GameId): Boolean
}

object GameKnowledge {
  def apply(): GameKnowledge = new testGameKnowledge()

  private[this] class testGameKnowledge() extends GameKnowledge {
    private val _availableGames: Set[GameId] = Set(BECCACCINO_GAME, BRISCOLA_GAME)

    def availableGames: Set[GameId] = this._availableGames

    def gameExists(game: GameId): Boolean = _availableGames contains game
  }

  private val BECCACCINO_GAME = GameId(1, "Beccaccino")
  private val BRISCOLA_GAME = GameId(1, "Briscola")


}
