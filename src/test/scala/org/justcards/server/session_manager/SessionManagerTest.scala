package org.justcards.server.session_manager

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.server.Commons.BriscolaSetting.BriscolaSetting
import org.justcards.server.Commons.Team._
import org.justcards.server.Commons.{BriscolaSetting, Team, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.user_manager.Lobby
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

class SessionManagerTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  import SessionManagerTest._
  import org.justcards.commons.AppError._

  private implicit val system: ActorSystem = ActorSystem("SessionManagerTest")

  val LOBBYID = 1
  val OWNERNAME = "Owner"
  val GAMEID = GameId("BECCACCINO")
  val SEED = "bastoni"
  val CARD = Card(1, SEED)


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
        me.expectMsgAllOf(GameStarted(List()), GameStarted(List()), GameStarted(List()), GameStarted(List())) //TODO
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
        val gameKnowledge = TestGameKnowledge(BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        me.expectMsg(ChooseBriscola(gameKnowledge.seeds, SessionManager.TIMEOUT_TO_USER)) //TODO
      }

      "communicate the briscola to all users after timeout for user decision" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.USER)))
        me receiveN 9
        me.within(SessionManager.TIMEOUT + 1 second) {
          me.expectMsgAllOf(CorrectBriscola(SEED), CorrectBriscola(SEED), CorrectBriscola(SEED), CorrectBriscola(SEED))
        }
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
      /*
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
      }*/

      "notify the player who win the match" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.NOT_BRISCOLA)))
        me receiveN 8
        matchExecution()
        me expectMsgAllOf(MatchWinner(TestGameKnowledge.endInfo._1, (TestGameKnowledge.endInfo._2,0), (TestGameKnowledge.endInfo._3,0)),
          MatchWinner(TestGameKnowledge.endInfo._1, (TestGameKnowledge.endInfo._2,0), (TestGameKnowledge.endInfo._3,0)),
          MatchWinner(TestGameKnowledge.endInfo._1, (TestGameKnowledge.endInfo._2,0), (TestGameKnowledge.endInfo._3,0)),
          MatchWinner(TestGameKnowledge.endInfo._1, (TestGameKnowledge.endInfo._2,0), (TestGameKnowledge.endInfo._3,0)))
        //TODO
      }

      "notify the player who win the session" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.NOT_BRISCOLA)))
        me receiveN 8
        matchExecution()
        me receiveN 4
        me expectMsgAllOf(GameWinner(TestGameKnowledge.endInfo._1),
          GameWinner(TestGameKnowledge.endInfo._1),
          GameWinner(TestGameKnowledge.endInfo._1),
          GameWinner(TestGameKnowledge.endInfo._1))
      }

      "notify the player that game is finished and they have to exit to lobby" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        system.actorOf(SessionManager(lobby, TestGameKnowledge(BriscolaSetting.NOT_BRISCOLA)))
        me receiveN 8
        matchExecution()
        me receiveN 8
        me expectMsgAllOf(OutOfLobby(lobby), OutOfLobby(lobby), OutOfLobby(lobby), OutOfLobby(lobby))
      }
    }
  }

  private def createLobby(implicit myRef: ActorRef): Lobby = {
    var lobby = Lobby(LOBBYID, UserInfo(OWNERNAME, myRef), GAMEID)
    (for (i <- 0 until 3; username = OWNERNAME + i)
      yield UserInfo(username, myRef)) foreach {
      userInfo => lobby = lobby + userInfo
    }
    lobby
  }

  private def matchExecution(): Unit = {
    //TODO
  }

}

object SessionManagerTest {



  object TestGameKnowledge {

    val SEED = "bastoni"
    val endInfo: (Team.Value, Int, Int) = (TEAM_1, 1, 0)

    def apply(briscolaSetting: BriscolaSetting.Value,
              correctBriscola: Boolean = true,
              playableCard: Boolean = true,
              firstHandWinner: UserInfo = null,
              sessionWinner: Boolean = true): GameKnowledge =
      TestGameKnowledge(briscolaSetting, correctBriscola, playableCard, firstHandWinner, sessionWinner)

    case class TestGameKnowledge(briscolaSetting: BriscolaSetting.Value,
                                 correctBriscola: Boolean,
                                 playableCard: Boolean,
                                 firstHandWinner: UserInfo,
                                 sessionWinner: Boolean
                                ) extends GameKnowledge {

      override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = (0,0,0)

      override def deckCards: Set[Card] = Set(Card(1, SEED))

      override def hasToChooseBriscola: BriscolaSetting = briscolaSetting

      override def setBriscola(seed: Seed): Boolean = correctBriscola

      override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = if (playableCard) Some(List()) else None

      override def handWinner(fieldCards: List[(Card, UserInfo)]): UserInfo = firstHandWinner

      override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = endInfo

      override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = if (sessionWinner) Some(endInfo._1) else None

      override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = (endInfo._2, endInfo._3)

      override def sessionStarterPlayer(playersHandCards: Set[(UserInfo, Set[Card])]): Option[UserInfo] = Some(playersHandCards.head._1)

      override def seeds: Set[Seed] = Set("denara", "spade", "bastoni", "coppe")
    }
  }

}