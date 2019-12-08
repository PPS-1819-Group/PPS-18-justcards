package org.justcards.server.knowledge_engine

import java.io.{File, PrintWriter}

import akka.actor.{Actor, Props}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.commons.GameId
import org.justcards.commons.games_rules.PointsConversion.PointsConversion
import org.justcards.commons.games_rules.GameRules
import org.justcards.commons.games_rules.converter.GameRulesConverter
import org.justcards.commons.games_rules.knowledge.RuleKnowledge
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}

class KnowledgeEngine(private val gameManager: GamesManager,
                      private val gameKnowledge: GameKnowledgeFactory) extends Actor {

  import KnowledgeEngine._

  override def receive: Receive = {
    case GameExistenceRequest(game) => sender() ! GameExistenceResponse(gameManager.gameExists(game))
    case RetrieveAvailableGames(_) => sender() ! AvailableGames(gameManager.availableGames)
    case GameKnowledgeRequest(game) =>
      val msg: Any =
        if(gameManager.gameExists(game)) GameKnowledgeResponse(gameKnowledge(game))
        else ErrorOccurred(GAME_NOT_EXISTING)
      sender() ! msg
    case CreateGame(name,rules) => gameManager createGame (name,rules) match {
      case Some(gameId) => sender() ! GameCreated(gameId)
      case _ => sender() ! ErrorOccurred(CANNOT_CREATE_GAME)
    }
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
  def createGame(name: String, rules: GameRules): Option[GameId]
}

object GamesManager {
  def apply(ruleKnowledge: RuleKnowledge, rulesConverter: GameRulesConverter): GamesManager =
    new FileGamesManager(ruleKnowledge)(rulesConverter)

  def apply(): GamesManager = GamesManager(RuleKnowledge(),GameRulesConverter())

   private[this] class FileGamesManager(private val ruleKnowledge: RuleKnowledge)
                                       (private implicit val rulesConverter: GameRulesConverter) extends GamesManager {

     import org.justcards.commons.games_rules.Rule._
     import org.justcards.commons.games_rules.converter.GameRulesConverter._

    private[this] val GAMES_PATH = GameKnowledge.GAMES_PATH

    private val games: Set[GameId] = readGames()

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game

    override def createGame(name: String, rules: GameRules): Option[GameId] = {
      val areRulesValid = List(
        isRuleValid[CardsDistribution](rules,CARDS_DISTRIBUTION),
        isRuleValid[Boolean](rules,PLAY_SAME_SEED),
        isRuleValid[BriscolaSetting](rules,CHOOSE_BRISCOLA),
        isRuleValid[Int](rules,POINTS_TO_WIN_SESSION),
        isRuleValid[PointsConversion](rules,POINTS_OBTAINED_IN_A_MATCH),
        isRuleValid[PointsConversion](rules,WINNER_POINTS),
        isRuleValid[PointsConversion](rules,LOSER_POINTS),
        isRuleValid[PointsConversion](rules,DRAW_POINTS),
        isRuleValid[Card](rules,STARTER_CARD),
        isRuleValid[Boolean](rules,LAST_TAKE_WORTH_ONE_MORE_POINT),
        isRuleValid[CardsHierarchyAndPoints](rules,CARDS_HIERARCHY_AND_POINTS),
      ) forall (v => v)
      if (areRulesValid) createGameFile(name, rulesConverter parse rules)
      else None
    }

     private def isRuleValid[X](rules: GameRules, rule: Rule[X])(implicit convert: String => Option[X]): Boolean = {
       rules get rule.toString match {
         case Some(value) =>
           val concreteValue: Option[X] = convert(value)
           if (concreteValue.isDefined) rule.isAllowed(concreteValue get) else false
         case _ => false
       }
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
     private def createGameFile(name: String, rules: List[String]): Option[GameId] = {
       try {
         val newGame = new File(GAMES_PATH + name + ".pl")
         val bw = new PrintWriter(newGame)
         rules foreach bw.println
         bw.close()
         Some(GameId(name))
       } catch {
         case _: Exception => None
       }
     }
   }
}