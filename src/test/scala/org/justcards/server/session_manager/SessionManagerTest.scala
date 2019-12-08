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
        val sessionManager = system.actorOf(SessionManager(lobby, TestGameKnowledge()))
        expectBroadcast(GameStarted(List((UserId(1,OWNERNAME),TEAM_1), (UserId(1,OWNERNAME + 1),TEAM_2), (UserId(1,OWNERNAME + 0),TEAM_1), (UserId(1,OWNERNAME + 2),TEAM_2))))
      }

      "send to users information of hand cards and field cards" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 4
        expectBroadcast(Information(Set(), List()))
      }

      "ask to an user the briscola if the game request this" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        me.expectMsg(ChooseBriscola(gameKnowledge.seeds, SessionManager.TIMEOUT_TO_USER))
      }

      "communicate the briscola to all users after timeout for user decision" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me.within(SessionManager.TIMEOUT + 1 second) {
          expectBroadcast(CorrectBriscola(SEED))
        }
      }

      "communicate the briscola to all users after user decision" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        expectBroadcast(CorrectBriscola(SEED))
      }

      "communicate that chosen briscola is not valid" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER, correctBriscola = false)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me.expectMsg(ErrorOccurred(BRISCOLA_NOT_VALID))
      }

      "communicate the briscola to all users after system selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.SYSTEM)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        expectBroadcast(CorrectBriscola(SEED))
      }

      "communicate the information and the turn to users after briscola selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge((1,0,0), BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me send (sessionManager, Briscola(SEED))
        me receiveN 4
        me.expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

      "communicate the information and the turn to users without briscola selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge((1,0,0))
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        me.expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

      "notify the player that the card played is playable" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge((1,0,0))
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me send (sessionManager, Play(CARD))
        me.expectMsg(Played(CARD))
      }

      "notify the player that the card played is not playable" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge((1,0,0), playableCard = false)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me send (sessionManager, Play(CARD))
        me.expectMsg(ErrorOccurred(CARD_NOT_VALID))
      }

      "notify the player who win the match" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        expectBroadcast(MatchWinner(TestGameKnowledge.endInfo._1, (TestGameKnowledge.endInfo._2,TestGameKnowledge.endInfo._3), (TestGameKnowledge.endInfo._2,TestGameKnowledge.endInfo._3)))
      }

      "notify the player who win the session" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        expectBroadcast(GameWinner(TestGameKnowledge.endInfo._1))
      }

      "notify the player that game is finished and they have to exit to lobby" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 16
        expectBroadcast(OutOfLobby(lobby))
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

  private def expectBroadcast(msg: AppMessage)(implicit me: TestProbe) = me expectMsgAllOf(msg,msg,msg,msg)

}

object SessionManagerTest {



  object TestGameKnowledge {

    val endInfo: (Team.Value, Int, Int) = (TEAM_1, 1, 0)
    val SEED = "bastoni"
    val CARD = Card(1, SEED)


    def apply(configuration:(Int, Int, Int) = (0,0,0),
              briscolaSetting: BriscolaSetting.Value = BriscolaSetting.NOT_BRISCOLA,
              correctBriscola: Boolean = true,
              playableCard: Boolean = true,
              firstHandWinner: UserInfo = null,
              sessionWinner: Boolean = true): GameKnowledge =
      TestGameKnowledge(configuration, briscolaSetting, correctBriscola, playableCard, firstHandWinner, sessionWinner)

    case class TestGameKnowledge(configuration: (Int,Int,Int),
                                 briscolaSetting: BriscolaSetting.Value,
                                 correctBriscola: Boolean,
                                 playableCard: Boolean,
                                 firstHandWinner: UserInfo,
                                 sessionWinner: Boolean
                                ) extends GameKnowledge {

      override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = configuration

      override def deckCards: Set[Card] = Set(Card(1, SEED), Card(1, SEED), Card(1, SEED), Card(1, SEED))

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