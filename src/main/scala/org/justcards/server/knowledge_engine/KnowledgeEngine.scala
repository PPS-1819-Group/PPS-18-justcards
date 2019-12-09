package org.justcards.server.knowledge_engine

import java.io.{File, PrintWriter}

import akka.actor.{Actor, Props}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.commons.GameId
import org.justcards.commons.games_rules.PointsConversion.PointsConversion
import org.justcards.commons.games_rules.GameRules
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.justcards.server.knowledge_engine.rule_creator.RuleCreator

class KnowledgeEngine(private val gameManager: GamesManager,
                      private val gameKnowledge: GameKnowledgeFactory,
                      private val rulesCreator: RuleCreator) extends Actor {

  import KnowledgeEngine._
  import org.justcards.commons.games_rules.Rule._

  override def receive: Receive = {
    case GameExistenceRequest(game) => sender() ! GameExistenceResponse(gameManager.gameExists(game))
    case RetrieveAvailableGames(_) => sender() ! AvailableGames(gameManager.availableGames)
    case GameKnowledgeRequest(game) =>
      val msg: Any =
        if(gameManager.gameExists(game)) GameKnowledgeResponse(gameKnowledge(game))
        else ErrorOccurred(GAME_NOT_EXISTING)
      sender() ! msg
    case CreateGameRequest(name,rules) =>
      if (areGameSettingsValid(name,rules)) {
        gameManager createGame (name, rulesCreator create rules) match {
          case Some(gameId) => sender() ! GameCreated(gameId)
          case _ => sender() ! ErrorOccurred(CANNOT_CREATE_GAME)
        }
      } else sender() ! ErrorOccurred(CANNOT_CREATE_GAME)
  }

  private def areGameSettingsValid(name: String, rules: GameRules): Boolean =
    !(gameManager gameExists GameId(name)) &&
    List(
        rules get CARDS_DISTRIBUTION.toString match {
          case Some((h: Int, d: Int, f: Int)) => CARDS_DISTRIBUTION isAllowed (h,d,f)
          case _ => false
        },
        rules get PLAY_SAME_SEED.toString match {
          case Some(value: Boolean) => PLAY_SAME_SEED isAllowed value
          case _ => false
        },
        rules get POINTS_TO_WIN_SESSION.toString match {
          case Some(v: Int) => POINTS_TO_WIN_SESSION isAllowed v
          case _ => false
        },
        rules get CHOOSE_BRISCOLA.toString match {
          case Some(v: BriscolaSetting) => CHOOSE_BRISCOLA isAllowed v
          case _ => false
        },
        isAllowedPointsRule(rules, POINTS_OBTAINED_IN_A_MATCH),
        rules get STARTER_CARD.toString match {
          case Some(v: Card) => STARTER_CARD isAllowed v
          case _ => false
        },
        isAllowedBooleanRule(rules, LAST_TAKE_WORTH_ONE_MORE_POINT),
        rules get CARDS_HIERARCHY_AND_POINTS.toString match {
          case Some(h :: t) => CARDS_HIERARCHY_AND_POINTS isAllowed (h :: t collect { case (number: Int, points: Int) => (number, points) })
          case _ => false
        },
        isAllowedPointsRule(rules, WINNER_POINTS),
        isAllowedPointsRule(rules, LOSER_POINTS),
        isAllowedPointsRule(rules, DRAW_POINTS)
      ).count(v=>v) == rules.size

  private def isAllowedBooleanRule(rules: GameRules, rule: Rule[Boolean]): Boolean = rules get rule.toString match {
    case Some(v: Boolean) => rule isAllowed v
    case _ => false
  }

  private def isAllowedPointsRule(rules: GameRules, rule: Rule[PointsConversion]): Boolean = rules get rule.toString match {
    case Some(v: PointsConversion) => rule isAllowed v
    case _ => false
  }

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
  def availableGames: Set[GameId]
  def gameExists(game: GameId): Boolean
  def createGame(name: String, rules: List[String]): Option[GameId]
}

object GamesManager {
  def apply(): GamesManager = new FileGamesManager()

   private[this] class FileGamesManager extends GamesManager {

    private[this] val GAMES_PATH = GameKnowledge.GAMES_PATH

    private val games: Set[GameId] = readGames()

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game

    override def createGame(name: String, rules: List[String]): Option[GameId] =
      try {
        val newGame = new File(GAMES_PATH + name + ".pl")
        val bw = new PrintWriter(newGame)
        rules foreach bw.println
        bw.close()
        Some(GameId(name))
      } catch {
        case _: Exception => None
      }

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