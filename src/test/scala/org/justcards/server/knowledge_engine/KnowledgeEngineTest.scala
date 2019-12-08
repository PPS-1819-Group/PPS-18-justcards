package org.justcards.server.knowledge_engine

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.commons.games_rules.GameRules
import org.justcards.server.Commons
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team.Team
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse, GameKnowledgeRequest, GameKnowledgeResponse}
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}

class KnowledgeEngineTest extends WordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  import KnowledgeEngineTest._

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

  }

}

object KnowledgeEngineTest {

  def createGamesManager(): GamesManager = new GamesManager {

    private val games = Set(BECCACCINO_GAME, BRISCOLA_GAME)

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game

    override def createGame(name: String, rules: GameRules): Option[GameId] = ???
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
  }

  private val BECCACCINO_GAME = GameId("Beccaccino")
  private val BRISCOLA_GAME = GameId("Briscola")
  private val NOT_EXISTING_GAME = GameId("Scopa")

}
