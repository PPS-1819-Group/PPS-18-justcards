package org.justcards.server.knowledge_engine

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}

class KnowledgeEngineTest extends WordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  import KnowledgeEngineTest._

  override def afterAll: Unit = {
    system terminate()
  }

  private implicit val system = ActorSystem("KnowledgeEngineTest")
  private val gameKnowledge = createGameKnowledge()

  "The knowledge engine" should {

    "return all the available games" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameKnowledge))
      knowledgeEngine ! RetrieveAvailableGames()
      me expectMsg(AvailableGames(Set(BECCACCINO_GAME, BRISCOLA_GAME)))
    }

    "know if a game exists" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameKnowledge))
      knowledgeEngine ! GameExistenceRequest(BECCACCINO_GAME)
      me expectMsg(GameExistenceResponse(true))
    }

    "know if a game doesn't exists" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val knowledgeEngine = system.actorOf(KnowledgeEngine(gameKnowledge))
      knowledgeEngine ! GameExistenceRequest(NOT_EXISTING_GAME)
      me expectMsg(GameExistenceResponse(false))
    }

  }

}

object KnowledgeEngineTest {

  def createGameKnowledge(): GameKnowledge = new GameKnowledge {

    private val games = Set(BECCACCINO_GAME, BRISCOLA_GAME)

    override def availableGames: Set[GameId] = games

    override def gameExists(game: GameId): Boolean = games contains game
  }

  private val BECCACCINO_GAME = GameId("Beccaccino")
  private val BRISCOLA_GAME = GameId("Briscola")
  private val NOT_EXISTING_GAME = GameId("Scopa")

}
