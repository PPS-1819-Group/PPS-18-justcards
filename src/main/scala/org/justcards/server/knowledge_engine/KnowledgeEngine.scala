package org.justcards.server.knowledge_engine

import java.io.{File, PrintWriter}
import java.util.Calendar

import akka.actor.{Actor, Props}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.commons.GameId
import org.justcards.commons.games_rules.PointsConversion.PointsConversion
import org.justcards.commons.games_rules.{GameRules, Rule}
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.justcards.server.knowledge_engine.rule_creator.RuleCreator

class KnowledgeEngine(private val gameManager: GamesManager,
                      private val gameKnowledge: GameKnowledgeFactory,
                      private val rulesCreator: RuleCreator) extends Actor {

  import KnowledgeEngine._
  import org.justcards.commons.games_rules.Rule._

  private val mandatoryRules = Rule.mandatoryRules

  override def receive: Receive = {
    case GameExistenceRequest(game) => sender() ! GameExistenceResponse(gameManager.gameExists(game))
    case RetrieveAvailableGames(_) => sender() ! AvailableGames(gameManager.availableGames)
    case GameKnowledgeRequest(game) =>
      val msg: Any =
        if(gameManager.gameExists(game)) GameKnowledgeResponse(gameKnowledge(game))
        else ErrorOccurred(GAME_NOT_EXISTING)
      sender() ! msg
    case CreateGameRequest(name,_) if name isBlank =>
      sender() ! ErrorOccurred(GAME_EMPTY_NAME)
    case CreateGameRequest(name,_) if gameManager gameExists GameId(name) =>
      sender() ! ErrorOccurred(GAME_ALREADY_EXISTS)
    case CreateGameRequest(_,rules) if !(mandatoryRules subsetOf rules.keySet) =>
      sender() ! ErrorOccurred(GAME_MISSING_RULES)
    case CreateGameRequest(name,rules) =>
      val wrongRules = nonValidRules(rules)
      if (wrongRules.isEmpty)
        gameManager createGame (name, rulesCreator create rules) match {
          case Some(gameId) => sender() ! GameCreated(gameId)
          case _ => sender() ! ErrorOccurred(CANNOT_CREATE_GAME)
        }
      else sender() ! ErrorOccurred(GAME_RULES_NOT_VALID)
  }

  private def nonValidRules(rules: GameRules): Set[Rule.Value] = rules filterNot {
    case (CARDS_DISTRIBUTION,(h:Int,d:Int,f:Int)) => CARDS_DISTRIBUTION isAllowed (h,d,f)
    case (PLAY_SAME_SEED,v: Boolean) => PLAY_SAME_SEED isAllowed v
    case (CHOOSE_BRISCOLA,v: BriscolaSetting) => CHOOSE_BRISCOLA isAllowed v
    case (POINTS_TO_WIN_SESSION,v :Int) => POINTS_TO_WIN_SESSION isAllowed v
    case (POINTS_OBTAINED_IN_A_MATCH,v: PointsConversion) => POINTS_OBTAINED_IN_A_MATCH isAllowed v
    case (WINNER_POINTS,v: PointsConversion) => WINNER_POINTS isAllowed v
    case (LOSER_POINTS,v: PointsConversion) => LOSER_POINTS isAllowed v
    case (DRAW_POINTS,v: PointsConversion) => DRAW_POINTS isAllowed v
    case (STARTER_CARD,v: Card) => STARTER_CARD isAllowed v
    case (LAST_TAKE_WORTH_ONE_MORE_POINT,v: Boolean) => LAST_TAKE_WORTH_ONE_MORE_POINT isAllowed v
    case (CARDS_HIERARCHY_AND_POINTS,v: List[Any]) =>
      CARDS_HIERARCHY_AND_POINTS isAllowed (v collect { case (number: Int, points: Int) => (number, points) })
    case _ => false
  } keySet

}

object KnowledgeEngine {
  def apply(gameManager: GamesManager, gameKnowledge: GameKnowledgeFactory, rulesCreator: RuleCreator): Props =
    Props(classOf[KnowledgeEngine], gameManager, gameKnowledge, rulesCreator)

  def apply(gameManager: GamesManager, gameKnowledge: GameKnowledgeFactory): Props =
    KnowledgeEngine(gameManager, gameKnowledge, RuleCreator())

  def apply(gameManager: GamesManager): Props = KnowledgeEngine(gameManager, GameKnowledge(), RuleCreator())

  case class GameExistenceRequest(gameId: GameId)
  case class GameExistenceResponse(existence: Boolean)
  case class GameKnowledgeRequest(game: GameId)
  case class GameKnowledgeResponse(gameKnowledgeInstance: GameKnowledge)
  case class CreateGameRequest(name: String, rules: GameRules)
}

trait GamesManager {
  def availableGames: Set[(GameId, Long)]
  def gameExists(game: GameId): Boolean
  def createGame(name: String, rules: List[String]): Option[GameId]
}

object GamesManager {
  def apply(): GamesManager = new FileGamesManager()

   private[this] class FileGamesManager extends GamesManager {

    private[this] val GAMES_PATH = GameKnowledge.GAMES_PATH

    private var games: Set[(GameId, Long)] = readGames()

    override def availableGames: Set[(GameId, Long)] = games

    override def gameExists(game: GameId): Boolean = games exists (_._1 == game)

    override def createGame(name: String, rules: List[String]): Option[GameId] =
      try {
        val newGame = new File(GAMES_PATH + name + ".pl")
        val bw = new PrintWriter(newGame)
        rules foreach bw.println
        bw.close()
        games = games + (GameId(name) -> Calendar.getInstance().getTimeInMillis)
        Some(GameId(name))
      } catch {
        case _: Exception => None
      }

    private def readGames(): Set[(GameId, Long)] = {
      val gameDirectory = new File(GAMES_PATH)
      gameDirectory.listFiles.toSet
        .filter(!_.isDirectory)
        .map(file => file.getName.split('.')(0) -> file.lastModified)
        .map(tuple => {
          val firstChar = tuple._1.charAt(0).toString
          tuple._1.replaceFirst(firstChar, firstChar.toUpperCase) -> tuple._2
        })
        .map(tuple => GameId(tuple._1) -> tuple._2)
    }
   }
}