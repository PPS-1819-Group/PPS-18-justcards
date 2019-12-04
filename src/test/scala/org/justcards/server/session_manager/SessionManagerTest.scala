package org.justcards.server.session_manager

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.TestProbe
import org.justcards.commons.{Briscola, Card, ChooseBriscola, CorrectBriscola, ErrorOccurred, GameId, GameStarted, Information, LobbyId, Play, Played, Turn}
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team._
import org.justcards.server.Commons.{BriscolaSetting, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.user_manager.Lobby
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}

class SessionManagerTest extends WordSpecLike with Matchers with BeforeAndAfter with BeforeAndAfterAll {

  import SessionManagerTest._
  import org.justcards.commons.AppError._

  private implicit val system: ActorSystem = ActorSystem("SessionManagerTest")
  private var tempActors: Set[ActorRef] = Set()

  val LOBBYID = 1
  val OWNERNAME = "Owner"
  val GAMEID = GameId("BECCACCINO")
  val TIMEOUT = 40
  val SEED = "bastoni"
  val CARD = Card(1, SEED)

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
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me.expectMsgAllOf(GameStarted(TEAM_1), GameStarted(TEAM_1), GameStarted(TEAM_2), GameStarted(TEAM_2))
    }

      "send to users information of hand cards and field cards" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me receiveN 4
        me.expectMsgAllOf(Information(Set(), List()), Information(Set(), List()), Information(Set(), List()), Information(Set(), List()))
      }

      "ask to an user the briscola if the game request this" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me receiveN 8
        me.expectMsg(ChooseBriscola(TIMEOUT))
      }

      "communicate the briscola to all users after user decision" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me.expectMsgAllOf(CorrectBriscola(SEED), CorrectBriscola(SEED), CorrectBriscola(SEED), CorrectBriscola(SEED))
      }

      "communicate that chosen briscola is not valid" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER, correctBriscola = false)))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me.expectMsg(ErrorOccurred(BRISCOLA_NOT_VALID))
      }

      "communicate the briscola to all users after system selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.SYSTEM)))
        me receiveN 8
        me.expectMsgAllOf(CorrectBriscola(SEED), CorrectBriscola(SEED), CorrectBriscola(SEED), CorrectBriscola(SEED))
      }

      "communicate the information and the turn to users after briscola selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me receiveN 4
        me.expectMsgAllOf(Turn(Set(), List(),TIMEOUT), Information(Set(), List()), Information(Set(), List()), Information(Set(), List()))
      }

      "communicate the information and the turn to users without briscola selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.NOT_BRISCOLA)))
        me receiveN 8
        me.expectMsgAllOf(Turn(Set(), List(),TIMEOUT), Information(Set(), List()), Information(Set(), List()), Information(Set(), List()))
      }

      "notify the player that the card played is playable" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me receiveN 8
        me send (sessionManager, Play(CARD))
        me.expectMsg(Played(CARD))
      }

      "notify the player that the card played is not playable" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER, playableCard = false)))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me receiveN 8
        me send (sessionManager, Play(CARD))
        me.expectMsg(ErrorOccurred(CARD_NOT_VALID))
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

  object TestGameKnowledge {

    val SEED = "bastoni"

    def apply(briscolaSetting: BriscolaSetting.Value,
              correctBriscola: Boolean = true,
              playableCard: Boolean = true,
              firstHandWinner: UserInfo = null): GameKnowledge =
      TestGameKnowledge(briscolaSetting, correctBriscola, playableCard, firstHandWinner)

    case class TestGameKnowledge(briscolaSetting: BriscolaSetting.Value,
                                 correctBriscola: Boolean,
                                 playableCard: Boolean,
                                 firstHandWinner: UserInfo
                                ) extends GameKnowledge {

      override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = (0,0,0)

      override def deckCards: Set[Card] = Set(Card(1, SEED))

      override def hasToChooseBriscola: BriscolaSetting = briscolaSetting

      override def setBriscola(seed: Seed): Boolean = correctBriscola

      override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = if (playableCard) Some(List()) else None

      override def handWinner(fieldCards: List[(Card, UserInfo)]): UserInfo = firstHandWinner

      override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = ???

      override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = ???

      override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = ???
    }
  }


}