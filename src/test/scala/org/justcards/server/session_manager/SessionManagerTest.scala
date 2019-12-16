package org.justcards.server.session_manager

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.commons.games_rules.BriscolaSetting.BriscolaSetting
import org.justcards.commons.games_rules.BriscolaSetting
import org.justcards.server.Commons.Team._
import org.justcards.server.Commons.{Team, UserInfo}
import org.justcards.server.knowledge_engine.game_knowledge.GameKnowledge
import org.justcards.server.session_manager.SessionManager.LogOutAndExitFromGame
import org.justcards.server.user_manager.Lobby
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

class SessionManagerTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  import SessionManagerTest._
  import org.justcards.commons.AppError._

  private implicit val system: ActorSystem = ActorSystem("SessionManagerTest")

  private val LOBBYID = 1
  private val OWNERNAME = "Owner"
  private val GAMEID = GameId("BECCACCINO")
  private val SEED = "bastoni"
  private val CARD = Card(1, SEED)


  override def afterAll: Unit = {
    system terminate()
  }

  "The Session Manager" when {

    "started" should {

      "make two teams" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        system.actorOf(SessionManager(lobby, TestGameKnowledge()))
        expectBroadcast(GameStarted(List((UserId(1, OWNERNAME), TEAM_1), (UserId(1, OWNERNAME + 1), TEAM_2), (UserId(1, OWNERNAME + 0), TEAM_1), (UserId(1, OWNERNAME + 2), TEAM_2))))
      }

      "send to users information of hand cards and field cards" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 4
        expectBroadcastClass(classOf[Information])
      }

      "ask to an user the briscola if the game request this" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER)
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        me expectMsg ChooseBriscola(gameKnowledge.seeds, SessionManager.TIMEOUT_TO_USER)
      }

      "communicate the briscola to all users after timeout for user decision" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER)
        system.actorOf(SessionManager(lobby, gameKnowledge))
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
        me send(sessionManager, Briscola(SEED))
        expectBroadcast(CorrectBriscola(SEED))
      }

      "communicate that chosen briscola is not valid" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER, correctBriscola = false)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me send(sessionManager, Briscola(SEED))
        me expectMsg ErrorOccurred(BRISCOLA_NOT_VALID)
      }

      "communicate the briscola to all users after system selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.SYSTEM)
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        expectBroadcastClass(classOf[CorrectBriscola])
      }

    }

    "the match start" should {

      "communicate the information and the turn to users after briscola selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(briscolaSetting = BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me send(sessionManager, Briscola(SEED))
        me receiveN 4
        me expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

      "communicate the information and the turn to users without briscola selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        me expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

      "notify the player that the card played is playable" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me send(sessionManager, Play(CARD))
        me expectMsg Played(CARD)
      }

      "don't allow another player to play instead of turn player" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        implicit val me2: TestProbe = TestProbe()
        implicit val myRef2: ActorRef = me.ref
        val owner = UserInfo(OWNERNAME + 0, myRef)
        var lobby = Lobby(LOBBYID, owner, GAMEID)
        for (i <- 1 to 2;
              username = OWNERNAME + i;
              userInfo = UserInfo(username, myRef))
          lobby = lobby + userInfo
        lobby = lobby + UserInfo(OWNERNAME + 3, myRef2)
        val gameKnowledge = TestGameKnowledge(firstPlayerTurn = owner)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me2 send(sessionManager, Play(CARD))
        me2.expectMsgType[ErrorOccurred]
      }

      "notify the player that the card played is not playable" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge(playableCard = false)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me send(sessionManager, Play(CARD))
        me expectMsg ErrorOccurred(CARD_NOT_VALID)
      }


      "play a card instead of user after timeout" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me.within(SessionManager.TIMEOUT + 1 second) {
          me.expectMsgType[Played]
        }
      }

      "communicate the information and the turn to users after first player play his card" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        me send(sessionManager, Play(CARD))
        me receiveN 1
        me expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

      "communicate the information and the turn to the right user after first player play his card" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        implicit val me2: TestProbe = TestProbe()
        implicit val myRef2: ActorRef = me2.ref
        val owner = UserInfo(OWNERNAME, myRef2)
        var lobby = Lobby(LOBBYID, owner, GAMEID)
        for (i <- 0 until 3;
             username = OWNERNAME + i;
             userInfo = UserInfo(username, myRef))
          lobby = lobby + userInfo
        val gameKnowledge = TestGameKnowledge(firstPlayerTurn = owner)
        system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        me2 receiveN 2
        me2.expectMsgType[Turn]
      }

      "allow all users to do their turn" in {
        implicit val meList: List[(TestProbe, ActorRef)] = for (i <- (1 to 4) toList; me = TestProbe()) yield (me, me.ref)
        val owner = UserInfo(OWNERNAME + 0, meList.head._2)
        var lobby = Lobby(LOBBYID, owner, GAMEID)
        for (i <- 1 to 3;
             username = OWNERNAME + i;
             userInfo = UserInfo(username, meList(i)._2))
          lobby = lobby + userInfo
        val gameKnowledge = TestGameKnowledge(firstPlayerTurn = owner)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        for (me <- meList)
          me._1 receiveN 2
        val msg0 = turnMessage(0, sessionManager)
        val msg2 = turnMessage(2, sessionManager)
        val msg1 = turnMessage(1, sessionManager)
        val msg3 = turnMessage(3, sessionManager)
        (msg0.isInstanceOf[Turn],
          msg1.isInstanceOf[Turn],
          msg2.isInstanceOf[Turn],
          msg3.isInstanceOf[Turn]) shouldBe(true, true, true, true)
      }

      "communicate the information of field to all users after hand" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge()
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 8
        for (i <- 0 until 4) {
          me receiveN 4
          me send(sessionManager, Play(CARD))
          me receiveN 1
        }
        me expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Information])
      }

      "notify players about who won the hand" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val handWinner = UserInfo(OWNERNAME, myRef)
        this completeHand lobby
        expectBroadcast(HandWinner(handWinner))
      }

      "communicate the information and the turn to users after first hand" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        this completeHand (lobby,(2,0,0))
        me receiveN 4
        me expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

      "allow user to draw card after first hand" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        this completeHand (lobby,(1,1,0))
        me receiveN 4
        me expectMsgAllClassOf(classOf[Information], classOf[Information], classOf[Information], classOf[Turn])
      }

    }

    "the match finish" should {

      "notify players that match is finished after they played all cards" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        this completeMatch lobby
        expectBroadcast(MatchWinner(TestGameKnowledge.endInfo._1, (TestGameKnowledge.endInfo._2, TestGameKnowledge.endInfo._3), (TestGameKnowledge.endInfo._2, TestGameKnowledge.endInfo._3)))
      }

    }

    "the session finish" should {

      "notify players about who won the session" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        this completeGame lobby
        expectBroadcast(GameWinner(TestGameKnowledge.endInfo._1))
      }

      "notify players that game is finished and they have to exit to lobby" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        this completeGame lobby
        me receiveN 4
        expectBroadcast(OutOfLobby(lobby))
      }
    }

    "an user disconnect" should {

      "replace him with a FakeUser" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge((1,0,0))
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 12
        sessionManager ! LogOutAndExitFromGame(UserInfo(OWNERNAME, myRef))
        me expectMsgAnyClassOf(classOf[Turn], classOf[Information])
      }

      "replace him with a FakeUser while option selection" in {
        implicit val me: TestProbe = TestProbe()
        implicit val myRef: ActorRef = me.ref
        val lobby = createLobby
        val gameKnowledge = TestGameKnowledge((1,0,0), BriscolaSetting.USER)
        val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
        me receiveN 9
        sessionManager ! LogOutAndExitFromGame(UserInfo(OWNERNAME, myRef))
        me.expectMsgType[CorrectBriscola]
      }
    }
  }

  private def createLobby(implicit myRef: ActorRef): Lobby = {
    var lobby = Lobby(LOBBYID, UserInfo(OWNERNAME, myRef), GAMEID)
    for (
      i <- 0 until 3;
      username = OWNERNAME + i;
      userInfo = UserInfo(username, myRef)
    ) lobby = lobby + userInfo
    lobby
  }

  private def turnMessage(indexTurn: Int, sessionManager: ActorRef) (implicit meList: List[(TestProbe, ActorRef)]): AnyRef = {
    val msg = (meList(indexTurn)._1 receiveN 1).head
    for (i <- 0 to 3 if i!=indexTurn)
      meList(i)._1 receiveN 1
    meList(indexTurn)._1 send(sessionManager, TimeoutExceeded())
    meList(indexTurn)._1 receiveN 1
    msg
  }

  private def completeHand(lobby: Lobby, conditions: (Int,Int,Int) = (1,0,0))(implicit me: TestProbe): Unit = {
    implicit val myRef = me.ref
    val handWinner = UserInfo(OWNERNAME, myRef)
    val gameKnowledge = TestGameKnowledge(conditions, firstHandWinner = handWinner)
    val sessionManager = system.actorOf(SessionManager(lobby, gameKnowledge))
    me receiveN 8
    for (i <- 0 until 4) {
      me receiveN 4
      me send(sessionManager, TimeoutExceeded())
      me receiveN 1
    }
    me receiveN 4
  }

  private def completeMatch(lobby: Lobby)(implicit me: TestProbe): Unit = {
    completeHand(lobby)
    me receiveN 4
  }

  private def completeGame(lobby: Lobby)(implicit me: TestProbe): Unit = {
    completeMatch(lobby)
    me receiveN 4
  }

  private def expectBroadcast(msg: AppMessage)(implicit me: TestProbe) = me expectMsgAllOf(msg,msg,msg,msg)
  private def expectBroadcastClass[X](msg: Class[X])(implicit me: TestProbe) =
    me expectMsgAllClassOf(msg, msg, msg, msg)

}

object SessionManagerTest {



  object TestGameKnowledge {

    val endInfo: (Team.Value, Int, Int) = (TEAM_1, 1, 0)
    val SEED = "bastoni"

    def apply(configuration:(Int, Int, Int) = (1,0,0),
              briscolaSetting: BriscolaSetting.Value = BriscolaSetting.NOT_BRISCOLA,
              firstPlayerTurn: UserInfo = null,
              correctBriscola: Boolean = true,
              playableCard: Boolean = true,
              firstHandWinner: UserInfo = null,
              sessionWinner: Boolean = true): GameKnowledge =
      TestGameKnowledge(configuration, briscolaSetting, firstPlayerTurn, correctBriscola, playableCard, firstHandWinner, sessionWinner)

    case class TestGameKnowledge(configuration: (Int,Int,Int),
                                 briscolaSetting: BriscolaSetting.Value,
                                 firstPlayerTurn: UserInfo,
                                 correctBriscola: Boolean,
                                 playableCard: Boolean,
                                 firstHandWinner: UserInfo,
                                 sessionWinner: Boolean
                                ) extends GameKnowledge {

      override def initialConfiguration: (CardsNumber, CardsNumber, CardsNumber) = configuration

      override def deckCards: Set[Card] = for (i <- (1 to 8) toSet) yield Card(i, SEED)

      override def hasToChooseBriscola: BriscolaSetting = briscolaSetting

      override def setBriscola(seed: Seed): Boolean = correctBriscola

      override def play(card: Card, fieldCards: List[Card], handCards: Set[Card]): Option[List[Card]] = if (playableCard) Some(List()) else None

      override def handWinner(fieldCards: List[(Card, UserInfo)]): UserInfo = firstHandWinner

      override def matchWinner(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Team, Points, Points) = endInfo

      override def sessionWinner(firstTeamPoints: Points, secondTeamPoints: Points): Option[Team] = if (sessionWinner) Some(endInfo._1) else None

      override def matchPoints(firstTeamCards: Set[Card], secondTeamCards: Set[Card], lastHandWinner: Team): (Points, Points) = (endInfo._2, endInfo._3)

      override def sessionStarterPlayer(playersHandCards: Set[(UserInfo, Set[Card])]): Option[UserInfo] = if (firstPlayerTurn != null) Some(firstPlayerTurn) else Some(playersHandCards.head._1)

      override def seeds: Set[Seed] = Set("denara", "spade", "bastoni", "coppe")
    }
  }

}