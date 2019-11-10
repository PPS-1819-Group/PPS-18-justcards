package org.justcards.server.user_manager

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons._
import org.justcards.server.knowledge_engine.KnowledgeEngine

class UserManagerLobbyTest extends TestKit(ActorSystem("UserManagerLobbyTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  import UserManagerLobbyTest._

  private val knowledgeEngine = system.actorOf(KnowledgeEngine())
  private var userManager: ActorRef = _

  before {
    userManager = system.actorOf(UserManager(knowledgeEngine))
  }

  after {
    userManager ! PoisonPill
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

      "allow to see the available lobbies" in {
        doLogIn(userManager, TEST_USERNAME)
        val msg = createLobby(userManager)
        expectLobbies(userManager, msg.lobby)
      }

      "not allow to see the available lobbies if not logged" in {
        createLobby(userManager)
        userManager ! LogOut(TEST_USERNAME)
        userManager ! RetrieveAvailableLobbies()
        expectMsgType[ErrorOccurred]
      }

    }

  }

  private def doLogIn(userManager: ActorRef, username: String): Unit = {
    userManager ! LogIn(username)
    receiveN(1)
  }

  private def createLobby(userManager: ActorRef): LobbyCreated = {
    doLogIn(userManager, TEST_USERNAME)
    userManager ! CreateLobby(GameId(GAME_TEST._1, GAME_TEST._2))
    receiveN(1).map(_.asInstanceOf[LobbyCreated]).head
  }

  private def expectLobbies(userManager: ActorRef, lobbies: LobbyId*): Unit = {
    expectLobbies(userManager, lobbies.toSet)
  }

  private def expectLobbies(userManager: ActorRef, lobbies: Set[LobbyId]): Unit = {
    userManager ! RetrieveAvailableLobbies()
    expectMsg(AvailableLobbies(lobbies))
  }

  private def expectNoLobby(userManager: ActorRef): Unit = expectLobbies(userManager)

}

object UserManagerLobbyTest {

  val TEST_USERNAME: String = "test-username"
  val GAME_TEST: (Int,String) = (1, "Beccaccino")

}
