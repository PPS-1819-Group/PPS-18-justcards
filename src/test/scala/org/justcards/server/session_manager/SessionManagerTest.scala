package org.justcards.server.session_manager

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.TestProbe
import org.justcards.commons.{Card, ChooseBriscola, GameId, GameStarted, Information, LobbyId}
import org.justcards.server.Commons
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team._
import org.justcards.server.Commons.{BriscolaSetting, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.{GameKnowledge, GameKnowledgeFactory}
import org.justcards.server.user_manager.Lobby
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}

class SessionManagerTest extends WordSpecLike with Matchers with BeforeAndAfter with BeforeAndAfterAll {

  import SessionManagerTest._

  private implicit val system = ActorSystem("SessionManagerTest")
  private var tempActors: Set[ActorRef] = Set()

  val LOBBYID = 1
  val OWNERNAME = "Owner"
  val GAMEID = GameId("BECCACCINO")
  val TIMEOUT = 40

  after {
    tempActors foreach {
      _ ! PoisonPill
    }
    tempActors = tempActors.empty
  }

  override def afterAll: Unit = {
    system terminate()
  }

  "The Session Manager" when {

    "started" should {

      "make two teams" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, createGameKnowledge()(lobby.game)))
        me.expectMsgAllOf(GameStarted(TEAM_1), GameStarted(TEAM_1), GameStarted(TEAM_2), GameStarted(TEAM_2))
    }

      "send to users information of hand cards and field cards" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, createGameKnowledge()(lobby.game)))
        me receiveN 4
        me.expectMsgAllOf(Information(Set(), List()), Information(Set(), List()), Information(Set(), List()), Information(Set(), List()))
      }

      "ask to an user the briscola if the game request this" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, createGameKnowledge()(lobby.game)))
        me receiveN 8
        me.expectMsg(ChooseBriscola(TIMEOUT))
      }


    }
  }

  private def createLobby(implicit myRef: ActorRef): Lobby = {
    var lobby = Lobby(LOBBYID, UserInfo(OWNERNAME, myRef), GAMEID )
    (for (i <- 0 until 3; username = OWNERNAME + i)
      yield UserInfo(username, myRef)) foreach {
      userInfo => lobby = lobby + userInfo
    }
    lobby
  }

}

object SessionManagerTest {

  val TEST_USERNAME: String = "test-username"
  val JOINER_USERNAME: String = "joiner-username"
  val GAME_TEST: String = "Beccaccino"
  val LOBBY_TEST: LobbyId = LobbyId(1, TEST_USERNAME, GameId(GAME_TEST))

  def createActor(testActor: ActorRef, sessionManager: ActorRef): Props =
    Props(classOf[SimpleActor], testActor: ActorRef, sessionManager)

  private[this] class SimpleActor(testActor: ActorRef, userManager: ActorRef) extends Actor {
    override def receive: Receive = {
      case _ =>
    }
  }

  def createGameKnowledge(): GameKnowledgeFactory = gameId => TestGameKnowledge(gameId)
  case class TestGameKnowledge(gameId: GameId) extends GameKnowledge {

    override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = (0, 0, 0)

    override def deckCards: Set[Card] = Set()

    override def hasToChooseBriscola: BriscolaSetting = BriscolaSetting.USER

    override def setBriscola(seed: Seed): Boolean = true

    override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = ???

    override def handWinner(fieldCards: List[(Card, Commons.UserInfo)]): Commons.UserInfo = ???

    override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = ???

    override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = ???

    override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = ???
  }


}