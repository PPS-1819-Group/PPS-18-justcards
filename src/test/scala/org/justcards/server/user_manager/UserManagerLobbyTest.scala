package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons.{LobbyJoined, _}
import org.justcards.server.knowledge_engine.KnowledgeEngine.{GameExistenceRequest, GameExistenceResponse}
import org.justcards.server.user_manager.UserManagerMessage.LogOutAndExitFromLobby

class UserManagerLobbyTest extends TestKit(ActorSystem("UserManagerLobbyTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  import UserManagerLobbyTest._

  private val knowledgeEngine = system.actorOf(createKnowledgeEngine(_ => true))
  private var userManager: ActorRef = _
  private var tempActors: Set[ActorRef] = Set()

  before {
    userManager = system.actorOf(UserManager(knowledgeEngine))
    tempActors = tempActors + userManager
  }

  after {
    tempActors foreach {
      _ ! PoisonPill
    }
    tempActors = tempActors.empty
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "The user manager" when {

    "created" should {

      "not contain any lobby" in {
        doLogIn(userManager, TEST_USERNAME)
        expectNoLobby(userManager)
      }

    }

    "is at runtime" should {

      "allow to create a lobby" in {
        doLogIn(userManager, TEST_USERNAME)
        userManager ! CreateLobby(GameId(GAME_TEST._1, GAME_TEST._2))
        expectMsgType[LobbyCreated]
      }

      "not allow to create a lobby if not logged" in {
        userManager ! CreateLobby(GameId(GAME_TEST._1, GAME_TEST._2))
        expectMsgType[ErrorOccurred]
      }

      "not allow to create a lobby if the game doesn't exist" in {
        val knowledgeEngineWithNoGames = system.actorOf(createKnowledgeEngine(_ => false))
        val myUserManager = system.actorOf(UserManager(knowledgeEngineWithNoGames))
        this.tempActors = this.tempActors ++ Set(knowledgeEngineWithNoGames, myUserManager)

        doLogIn(myUserManager, TEST_USERNAME)
        myUserManager ! CreateLobby(GameId(GAME_TEST._1, GAME_TEST._2))
        expectMsgType[ErrorOccurred]
      }

      "allow to see the available lobbies" in {
        doLogIn(userManager, TEST_USERNAME)
        val msg = createLobby(userManager)
        expectLobbies(userManager, (msg.lobby, Set(UserId(1, TEST_USERNAME))))
      }

      "not allow to see the available lobbies if not logged" in {
        doLogIn(userManager, TEST_USERNAME)
        createLobby(userManager)
        userManager ! LogOut(TEST_USERNAME)
        userManager ! RetrieveAvailableLobbies()
        expectMsgType[ErrorOccurred]
      }

      "not allow to see a lobby if it's full" in {
        doLogIn(userManager, TEST_USERNAME)
        val lobbyInfo = createLobby(userManager)
        fillLobby(lobbyInfo.lobby)
        expectNoLobby(userManager)
      }

      "allow to join a lobby" in {
        val joiner = createJoinerAndLogIn(userManager, JOINER_USERNAME)
        this.tempActors = this.tempActors + joiner
        doLogIn(userManager, TEST_USERNAME)
        val lobbyInfo = createLobby(userManager)
        joiner ! JoinLobby(lobbyInfo.lobby)
        val users = Set(UserId(1, TEST_USERNAME), UserId(1, JOINER_USERNAME))
        val messages = receiveN(2)
        messages should contain (LobbyJoined(lobbyInfo.lobby, users))
      }

      "inform all the members of a lobby when a new user joins" in {
        val joiner1 = createJoinerAndLogIn(userManager, JOINER_USERNAME)
        val joiner2 = createJoinerAndLogIn(userManager, JOINER_USERNAME + "2")
        this.tempActors = this.tempActors ++ Set(joiner1,joiner2)
        doLogIn(userManager, TEST_USERNAME)
        val lobbyInfo = createLobby(userManager)
        joiner1 ! JoinLobby(lobbyInfo.lobby)
        receiveN(2)
        joiner2 ! JoinLobby(lobbyInfo.lobby)
        val users = Set(UserId(1, TEST_USERNAME), UserId(1, JOINER_USERNAME), UserId(1, JOINER_USERNAME + 2))
        expectMsgAllOf(
          LobbyJoined(lobbyInfo.lobby, users),
          LobbyUpdate(lobbyInfo.lobby, users),
          LobbyUpdate(lobbyInfo.lobby, users)
        )
      }

      "not allow to join a lobby if not logged" in {
        userManager ! JoinLobby(LobbyId(1))
        expectMsgType[ErrorOccurred]
      }

      "not allow to join a lobby if the lobby doesn't exist" in {
        doLogIn(userManager, TEST_USERNAME)
        userManager ! JoinLobby(LobbyId(1))
        expectMsgType[ErrorOccurred]
      }

      "not allow to join a lobby if the user is already in another lobby" in {
        doLogIn(userManager, TEST_USERNAME)
        val lobbyId = createLobby(userManager)
        userManager ! JoinLobby(lobbyId.lobby)
        expectMsgType[ErrorOccurred]
      }

      "not allow to join a lobby if it reached maximum capacity" in {
        /* create lobby and let enter n-1 people */
        doLogIn(userManager, TEST_USERNAME)
        val lobbyInfo = createLobby(userManager)
        this.tempActors = this.tempActors ++ fillLobby(lobbyInfo.lobby)
        /* now the lobby is full */
        val lastJoiner = createJoinerAndLogIn(userManager, JOINER_USERNAME + "-illegal")
        this.tempActors = this.tempActors + lastJoiner
        lastJoiner ! JoinLobby(lobbyInfo.lobby)
        expectMsgType[ErrorOccurred]
      }

      "return all the current members of a lobby" in {
        val joiner = createJoinerAndLogIn(userManager, JOINER_USERNAME)
        this.tempActors = this.tempActors + joiner
        doLogIn(userManager, TEST_USERNAME)
        val lobbyInfo = createLobby(userManager)
        joiner ! JoinLobby(lobbyInfo.lobby)
        receiveN(2)
        userManager ! RetrieveAvailableLobbies()
        val tuple = lobbyInfo.lobby -> Set(UserId(1, TEST_USERNAME), UserId(1, JOINER_USERNAME))
        expectMsg(AvailableLobbies(Set(tuple)))
      }

      "exit a user from a lobby if it logs out" in {
        doLogIn(userManager, TEST_USERNAME)
        val joiner = createJoinerAndLogIn(userManager, JOINER_USERNAME)
        this.tempActors = this.tempActors + joiner
        val lobbyInfo = createLobby(userManager)
        joiner ! JoinLobby(lobbyInfo.lobby)
        receiveN(2)
        userManager ! LogOutAndExitFromLobby(TEST_USERNAME, lobbyInfo.lobby)
        expectMsg(LobbyUpdate(lobbyInfo.lobby, Set(UserId(1, JOINER_USERNAME))))
      }

      "delete a lobby if it remains empty" in {
        doLogIn(userManager, TEST_USERNAME)
        val lobbyInfo = createLobby(userManager)
        userManager ! LogOutAndExitFromLobby(TEST_USERNAME, lobbyInfo.lobby)
        doLogIn(userManager, TEST_USERNAME)
        expectNoLobby(userManager)
      }

    }

  }

  private def doLogIn(userManager: ActorRef, username: String): Unit = {
    userManager ! LogIn(username)
    receiveN(1)
  }

  private def createJoinerAndLogIn(userManager: ActorRef, username: String): ActorRef = {
    val joiner = system.actorOf(createActor(testActor, userManager))
    joiner ! LogIn(username)
    receiveN(1)
    joiner
  }

  private def createLobby(userManager: ActorRef): LobbyCreated = {
    userManager ! CreateLobby(GameId(GAME_TEST._1, GAME_TEST._2))
    receiveN(1).map(_.asInstanceOf[LobbyCreated]).head
  }

  private def fillLobby(lobbyInfo: LobbyId): Seq[ActorRef] = {
    for(
      n <- 0 until Lobby.MAX_LOBBY_MEMBERS - 1;
      joiner = createJoinerAndLogIn(userManager, JOINER_USERNAME + n);
      _ = joiner ! JoinLobby(lobbyInfo);
      _ = receiveN(n + 2)
    ) yield joiner
  }

  private def expectLobbies(userManager: ActorRef, lobbies: (LobbyId, Set[UserId])*): Unit = {
    expectLobbies(userManager, lobbies.toSet)
  }

  private def expectLobbies(userManager: ActorRef, lobbies: Set[(LobbyId, Set[UserId])]): Unit = {
    userManager ! RetrieveAvailableLobbies()
    expectMsg(AvailableLobbies(lobbies))
  }

  private def expectNoLobby(userManager: ActorRef): Unit = expectLobbies(userManager)

}

object UserManagerLobbyTest {

  val TEST_USERNAME: String = "test-username"
  val JOINER_USERNAME: String = "joiner-username"
  val GAME_TEST: (Int,String) = (1, "Beccaccino")

  def createActor(testActor: ActorRef, userManager: ActorRef): Props =
    Props(classOf[SimpleActor], testActor: ActorRef, userManager)

  def createKnowledgeEngine(operation: GameExistenceRequest => Boolean): Props =
    Props(classOf[KnowledgeEngineStub], operation)

  private[this] class SimpleActor(testActor: ActorRef, userManager: ActorRef) extends Actor {
    override def receive: Receive = {
      case a: Logged => testActor ! a
      case a: LobbyJoined => testActor ! a
      case a: LobbyUpdate => testActor ! a
      case a: ErrorOccurred => testActor ! a
      case msg => userManager ! msg
    }
  }

  private[this] class KnowledgeEngineStub(acceptMessage: GameExistenceRequest => Boolean) extends Actor {
    override def receive: Receive = {
      case msg: GameExistenceRequest => sender() ! GameExistenceResponse(acceptMessage(msg))
    }
  }

}
