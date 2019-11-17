package org.justcards.server.knowledge_engine

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse, GameKnowledgeRequest, GameKnowledgeResponse}
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}

class KnowledgeEngineTest extends TestKit(ActorSystem("KnowledgeEngineTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll with BeforeAndAfter{

  import KnowledgeEngineTest._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  private val gameManager = createGamesManager()
  private val gameKnowledge = createGameKnowledge()
  private val knowledgeEngine = system.actorOf(KnowledgeEngine(gameManager, gameKnowledge))

  "The knowledge engine" should {

    "return all the available games" in {
      knowledgeEngine ! RetrieveAvailableGames()
      expectMsg(AvailableGames(Set(BECCACCINO_GAME, BRISCOLA_GAME)))
    }

    "know if a game exists" in {
      knowledgeEngine ! GameExistenceRequest(BECCACCINO_GAME)
      expectMsg(GameExistenceResponse(true))
    }

    "know if a game doesn't exists" in {
      knowledgeEngine ! GameExistenceRequest(NOT_EXISTING_GAME)
      expectMsg(GameExistenceResponse(false))
    }

    "return the correct game knowledge when asked to" in {
      knowledgeEngine ! GameKnowledgeRequest(BECCACCINO_GAME)
      val myGameKnowledge = createGameKnowledge()(BECCACCINO_GAME)
      expectMsg(GameKnowledgeResponse(myGameKnowledge))
    }

  }

}

object KnowledgeEngineTest {

  def createGamesManager(): GamesManager = new GamesManager {

    private val games = Set(BECCACCINO_GAME, BRISCOLA_GAME)

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game
  }

  def createGameKnowledge(): GameKnowledgeFactory = gameId => new TestGameKnowledge(gameId)
  case class TestGameKnowledge(gameId: GameId) extends GameKnowledge

  private val BECCACCINO_GAME = GameId("Beccaccino")
  private val BRISCOLA_GAME = GameId("Briscola")
  private val NOT_EXISTING_GAME = GameId("Scopa")

}