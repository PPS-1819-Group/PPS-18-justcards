package org.justcards.server.knowledge_engine

import java.io.File

import akka.actor.{Actor, Props}
import org.justcards.commons._
import org.justcards.commons.GameId
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory, PrologGameKnowledge}

class KnowledgeEngine(private val gameManager: GamesManager,
                      private val gameKnowledge: GameKnowledgeFactory) extends Actor {

  import KnowledgeEngine._

  override def receive: Receive = {
    case GameExistenceRequest(game) => sender() ! GameExistenceResponse(gameManager.gameExists(game))
    case RetrieveAvailableGames(_) => sender() ! AvailableGames(gameManager.availableGames)
    case GameKnowledgeRequest(game) =>
      val msg: Any =
        if(gameManager.gameExists(game)) GameKnowledgeResponse(gameKnowledge(game))
        else ErrorOccurred(AppError.GAME_NOT_EXISTING)
      sender() ! msg
  }

}

object KnowledgeEngine {
  def apply(gameManager: GamesManager, gameKnowledge: GameKnowledgeFactory): Props =
    Props(classOf[KnowledgeEngine], gameManager, gameKnowledge)

  def apply(gameManager: GamesManager): Props =
    Props(classOf[KnowledgeEngine], gameManager, GameKnowledge())

  case class GameExistenceRequest(gameId: GameId)
  case class GameExistenceResponse(existence: Boolean)
  case class GameKnowledgeRequest(game: GameId)
  case class GameKnowledgeResponse(gameKnowledgeInstance: GameKnowledge)
}

trait GamesManager {
  def availableGames: Set[GameId]
  def gameExists(game: GameId): Boolean
}

object GamesManager {
  def apply(): GamesManager = new FileGamesManager()

   private[this] class FileGamesManager extends GamesManager {

    private[this] val GAMES_PATH = GameKnowledge.GAMES_PATH

    private val games: Set[GameId] = readGames()

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game

    private def readGames(): Set[GameId] = {
      val gameDirectory = new File(GAMES_PATH)
      gameDirectory.listFiles.toSet
        .filter(!_.isDirectory)
        .map(_.getName.split('.')(0))
        .map(name => {
          val firstChar = name.charAt(0).toString
          name.replaceFirst(firstChar, firstChar.toUpperCase)
        })
        .map(GameId)
    }
  }
}
