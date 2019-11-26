package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.server.Commons.UserInfo
import org.justcards.server.user_manager.UserManagerMessage._

class UserManagerPlayerTest extends TestKit(ActorSystem("UserManagerPlayerTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll with BeforeAndAfter {

  import UserManagerPlayerTest._

  private var userManager: ActorRef = _
  private val knowledgeEngine = system.actorOf(createKnowledgeEngine())

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

      "not contain any logged user" in {
        expectNoUsers(userManager)
      }

    }

    "is at runtime" should {

      "register a user" in {
        userManager ! LogIn(TEST_USERNAME)
        expectMsgType[Logged]
      }

      "save the user after its registration" in {
        doLogIn(userManager, TEST_USERNAME)
        expectUsers(userManager, UserInfo(TEST_USERNAME, testActor))
      }

      "not register a user if the username is already present" in {
        doLogIn(userManager, TEST_USERNAME)
        userManager ! LogIn(TEST_USERNAME)
        expectMsg(ErrorOccurred(USER_ALREADY_PRESENT))
      }

      "not allow to the same user to register twice with different username" in {
        doLogIn(userManager, DOUBLE_USER_USERNAME)
        userManager ! LogIn(DOUBLE_USER_USERNAME + "-retry")
        expectMsg(ErrorOccurred(USER_ALREADY_LOGGED))
      }

      "unregister a user" in {
        doLogIn(userManager, TEST_USERNAME)
        userManager ! UserLogout(TEST_USERNAME, testActor)
        expectNoMessage()
        expectNoUsers(userManager)
      }

      "return the current logged users" in {
        val actorUsernameSet = for(
          n: Int <- (0 to 4).toSet;
          actor: ActorRef = system.actorOf(createActor(testActor, userManager));
          message: String = MULTI_TEST_USERNAME + n
        ) yield (actor, message)

        actorUsernameSet.foreach(t => t._1 ! LogIn(t._2))
        receiveN(actorUsernameSet.size)
        expectUsers(userManager, actorUsernameSet map (tuple => UserInfo(tuple._2, tuple._1)))
      }

      "ask for available games to the knowledge engine" in {
        userManager ! RetrieveAvailableGames()
        expectMsg(KnowledgeEngineMsg(RetrieveAvailableGames()))
      }

    }

  }

  private def doLogIn(userManager: ActorRef, username: String): Unit = {
    userManager ! LogIn(username)
    receiveN(1)
  }

  private def expectUsers(userManager: ActorRef, users: UserInfo*): Unit = {
    expectUsers(userManager, users.toSet)
  }

  private def expectUsers(userManager: ActorRef, users: Set[UserInfo]): Unit = {
    userManager ! RetrieveAllPlayers(testActor)
    expectMsg(Players(users))
  }

  private def expectNoUsers(userManager: ActorRef): Unit = expectUsers(userManager)

}

object UserManagerPlayerTest {

  val TEST_USERNAME: String = "test-username"
  val MULTI_TEST_USERNAME: String = "multi-test-username"
  val DOUBLE_USER_USERNAME: String = "double-user"

  case class KnowledgeEngineMsg(msg: Any)

  def createActor(testActor: ActorRef, userManager: ActorRef): Props =
    Props(classOf[SimpleActor], testActor, userManager)

  def createKnowledgeEngine(): Props = Props(classOf[SimpleKnowledgeEngine])

  private[this] class SimpleActor(testActor: ActorRef, userManager: ActorRef) extends Actor {
    override def receive: Receive = {
      case a: Logged => testActor ! a
      case msg => userManager ! msg
    }
  }

  private[this] class SimpleKnowledgeEngine() extends Actor {
    override def receive: Receive = {
      case m: RetrieveAvailableGames => sender() ! KnowledgeEngineMsg(m)
    }
  }

}
