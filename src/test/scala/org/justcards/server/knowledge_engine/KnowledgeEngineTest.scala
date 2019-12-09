package org.justcards.server.knowledge_engine

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.commons.games_rules.{GameRules, PointsConversion}
import org.justcards.server.Commons
import org.justcards.server.Commons.BriscolaSetting
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.KnowledgeEngine.{CreateGameRequest, GameExistenceRequest, GameExistenceResponse, GameKnowledgeRequest, GameKnowledgeResponse}
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.justcards.server.knowledge_engine.rule_creator.RuleCreator
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}

class KnowledgeEngineTest extends WordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  import KnowledgeEngineTest._
  import org.justcards.commons.games_rules.Rule._

  private implicit val system = ActorSystem("KnowledgeEngineTest")
  private val gameManager = createGamesManager()
  private val gameKnowledge = createGameKnowledge()

  override def afterAll: Unit = {
    system terminate()
  }

  "The knowledge engine" should {

    "return all the available games" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge))
      knowledgeEngine ! RetrieveAvailableGames()
      me expectMsg AvailableGames(Set(BECCACCINO_GAME, BRISCOLA_GAME))
    }

    "know if a game exists" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge))
      knowledgeEngine ! GameExistenceRequest(BECCACCINO_GAME)
      me expectMsg GameExistenceResponse(true)
    }

    "know if a game doesn't exists" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge))
      knowledgeEngine ! GameExistenceRequest(NOT_EXISTING_GAME)
      me expectMsg GameExistenceResponse(false)
    }

    "return the correct game knowledge when asked to" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge))
      knowledgeEngine ! GameKnowledgeRequest(BECCACCINO_GAME)
      val myGameKnowledge = createGameKnowledge()(BECCACCINO_GAME)
      me expectMsg GameKnowledgeResponse(myGameKnowledge)
    }

    "return a game id if game has been created" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge, RuleCreator()))
      val rules: GameRules = Map(
        PLAY_SAME_SEED.toString -> true,
        POINTS_TO_WIN_SESSION.toString -> 41,
        CARDS_DISTRIBUTION.toString -> ((10,0,0)),
        CHOOSE_BRISCOLA.toString -> BriscolaSetting.USER,
        POINTS_OBTAINED_IN_A_MATCH.toString -> PointsConversion.DIVIDE.value(3),
        STARTER_CARD.toString -> Card(4,"denara"),
        LAST_TAKE_WORTH_ONE_MORE_POINT.toString -> true,
        CARDS_HIERARCHY_AND_POINTS.toString -> List((3,1),(2,1),(1,3),(10,1),(9,1),(8,1),(7,0),(6,0),(5,0),(4,0)).reverse,
        WINNER_POINTS.toString -> PointsConversion.MATCH_POINTS,
        LOSER_POINTS.toString -> PointsConversion.MATCH_POINTS,
        DRAW_POINTS.toString -> PointsConversion.EXACTLY.value(0)
      )
      knowledgeEngine ! CreateGameRequest(ALLOWED_NAME, rules)
      me expectMsg GameCreated(GameId(ALLOWED_NAME))
    }

    "return an error if game has not been created" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge))
      knowledgeEngine ! CreateGameRequest(NOT_ALLOWED_NAME, Map())
      me expectMsg ErrorOccurred(CANNOT_CREATE_GAME)
    }

  }

}

object KnowledgeEngineTest {

  def createGamesManager(): GamesManager = new GamesManager {

    private val games = Set(BECCACCINO_GAME, BRISCOLA_GAME)

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game

    override def createGame(name: String, rules: List[String]): Option[GameId] = if (name == ALLOWED_NAME) Some(GameId(name)) else None
  }

  def createGameKnowledge(): GameKnowledgeFactory = gameId => TestGameKnowledge(gameId)
  case class TestGameKnowledge(gameId: GameId) extends GameKnowledge {

    override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = ???

    override def deckCards: Set[Card] = ???

    override def hasToChooseBriscola: BriscolaSetting = ???

    override def setBriscola(seed: Seed): Boolean = ???

    override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = ???

    override def handWinner(fieldCards: List[(Card, Commons.UserInfo)]): Commons.UserInfo = ???

    override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = ???

    override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = ???

    override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = ???

    override def sessionStarterPlayer(playersHandCards: Set[(Commons.UserInfo, Set[Card])]): Option[Commons.UserInfo] = ???

    override def seeds: Set[Seed] = ???
  }

  private val BECCACCINO_GAME = GameId("Beccaccino")
  private val BRISCOLA_GAME = GameId("Briscola")
  private val NOT_EXISTING_GAME = GameId("Scopa")
  private val ALLOWED_NAME = "name"
  private val NOT_ALLOWED_NAME = ""

}
