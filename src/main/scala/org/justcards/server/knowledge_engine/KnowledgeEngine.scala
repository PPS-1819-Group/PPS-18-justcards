package org.justcards.server.knowledge_engine

import java.io.{File, PrintWriter}

import akka.actor.{Actor, Props}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.commons.GameId
import org.justcards.commons.games_rules.PointsConversion.PointsConversion
import org.justcards.commons.games_rules.GameRules
import org.justcards.commons.games_rules.converter.GameRulesConverter
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.justcards.server.knowledge_engine.rule_creator.RuleCreator

class KnowledgeEngine(private val gameManager: GamesManager,
                      private val gameKnowledge: GameKnowledgeFactory,
                      private val rulesCreator: RuleCreator,
                      private implicit val rulesConverter: GameRulesConverter) extends Actor {

  import KnowledgeEngine._
  import org.justcards.commons.games_rules.Rule._
  import org.justcards.commons.games_rules.converter.GameRulesConverter._

  override def receive: Receive = {
    case GameExistenceRequest(game) => sender() ! GameExistenceResponse(gameManager.gameExists(game))
    case RetrieveAvailableGames(_) => sender() ! AvailableGames(gameManager.availableGames)
    case GameKnowledgeRequest(game) =>
      val msg: Any =
        if(gameManager.gameExists(game)) GameKnowledgeResponse(gameKnowledge(game))
        else ErrorOccurred(GAME_NOT_EXISTING)
      sender() ! msg
    case CreateGame(name,rules) =>
      val validRules = getRulesIfGameIsValid(name,rules)
      if (validRules.isDefined)
        gameManager createGame (name, rulesCreator create validRules.get) match {
          case Some(gameId) => sender() ! GameCreated(gameId)
          case _ => sender() ! ErrorOccurred(CANNOT_CREATE_GAME)
        }
      else sender() ! ErrorOccurred(CANNOT_CREATE_GAME)
  }

  private def getRulesIfGameIsValid(name: String, rules: GameRules): Option[Map[String,Any]] = {
    if (gameManager gameExists GameId(name)) None
    else {
      val validRules = Set(
        getRuleIfValid[CardsDistribution](rules,CARDS_DISTRIBUTION),
        getRuleIfValid[Boolean](rules,PLAY_SAME_SEED),
        getRuleIfValid[BriscolaSetting](rules,CHOOSE_BRISCOLA),
        getRuleIfValid[Int](rules,POINTS_TO_WIN_SESSION),
        getRuleIfValid[PointsConversion](rules,POINTS_OBTAINED_IN_A_MATCH),
        getRuleIfValid[PointsConversion](rules,WINNER_POINTS),
        getRuleIfValid[PointsConversion](rules,LOSER_POINTS),
        getRuleIfValid[PointsConversion](rules,DRAW_POINTS),
        getRuleIfValid[Card](rules,STARTER_CARD),
        getRuleIfValid[Boolean](rules,LAST_TAKE_WORTH_ONE_MORE_POINT),
        getRuleIfValid[CardsHierarchyAndPoints](rules,CARDS_HIERARCHY_AND_POINTS)
      ).collect{ case Some(v) => v }.toMap;
      if (validRules.size == rules.size) Some(validRules) else None
    }
  }

  private def getRuleIfValid[X](rules: GameRules, rule: Rule[X])(implicit convert: String => Option[X]): Option[(String,X)] = {
    rules get rule.toString match {
      case Some(value) =>
        val concreteValue: Option[X] = convert(value)
        if (concreteValue.isDefined && rule.isAllowed(concreteValue get)) Some((rule.toString,concreteValue get)) else None
      case _ => None
    }
  }

}

object KnowledgeEngine {
  def apply(gameManager: GamesManager, gameKnowledge: GameKnowledgeFactory, rulesCreator: RuleCreator, rulesConverter: GameRulesConverter): Props =
    Props(classOf[KnowledgeEngine], gameManager, gameKnowledge, rulesCreator, rulesConverter)

  def apply(gameManager: GamesManager, gameKnowledge: GameKnowledgeFactory): Props =
    KnowledgeEngine(gameManager, gameKnowledge, RuleCreator(), GameRulesConverter())

  def apply(gameManager: GamesManager): Props = KnowledgeEngine(gameManager, GameKnowledge(), RuleCreator(), GameRulesConverter())

  case class GameExistenceRequest(gameId: GameId)
  case class GameExistenceResponse(existence: Boolean)
  case class GameKnowledgeRequest(game: GameId)
  case class GameKnowledgeResponse(gameKnowledgeInstance: GameKnowledge)
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