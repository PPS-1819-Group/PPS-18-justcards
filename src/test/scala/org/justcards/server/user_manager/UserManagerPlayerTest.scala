package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.server.Commons.UserInfo
import org.justcards.server.user_manager.UserManagerMessage._
import org.justcards.server.user_manager.Utils.doLogIn

class UserManagerPlayerTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  import UserManagerPlayerTest._

  private implicit val system = ActorSystem("UserManagerPlayerTest")
  private val knowledgeEngine = system.actorOf(createKnowledgeEngine())

  override def afterAll: Unit = {
    system terminate()
  }

  "The user manager" when {

    "created" should {

      "not contain any logged user" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        expectNoUsers(userManager)
      }

    }

    "is at runtime" should {

      "register a user" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        userManager ! LogIn(TEST_USERNAME)
        me.expectMsgType[Logged]
      }

      "save the user after its registration" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        doLogIn(userManager, TEST_USERNAME)
        expectUsers(userManager, UserInfo(TEST_USERNAME, myRef))
      }

      "not register a user if the username is already present" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        doLogIn(userManager, TEST_USERNAME)
        userManager ! LogIn(TEST_USERNAME)
        me expectMsg(ErrorOccurred(USER_ALREADY_PRESENT))
      }

      "not allow to the same user to register twice with different username" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        doLogIn(userManager, DOUBLE_USER_USERNAME)
        userManager ! LogIn(DOUBLE_USER_USERNAME + "-retry")
        me expectMsg(ErrorOccurred(USER_ALREADY_LOGGED))
      }

      "unregister a user" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        doLogIn(userManager, TEST_USERNAME)
        userManager ! UserLogout(TEST_USERNAME, myRef)
        me expectNoMessage()
        expectNoUsers(userManager)
      }

      "return the current logged users" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        val actorUsernameSet = for(
          n: Int <- (0 to 4).toSet;
          actor: ActorRef = system.actorOf(createActor(myRef, userManager));
          message: String = MULTI_TEST_USERNAME + n
        ) yield (actor, message)

        actorUsernameSet.foreach(t => t._1 ! LogIn(t._2))
        me receiveN(actorUsernameSet.size)
        expectUsers(userManager, actorUsernameSet map (tuple => UserInfo(tuple._2, tuple._1)))
      }

      "ask for available games to the knowledge engine" in {
        implicit val me = TestProbe()
        implicit val myRef = me.ref
        val userManager = system.actorOf(UserManager(myRef, knowledgeEngine))
        userManager ! RetrieveAvailableGames()
        me expectMsg(KnowledgeEngineMsg(RetrieveAvailableGames()))
      }

    }

  }

  private def expectUsers(userManager: ActorRef, users: UserInfo*)(implicit me: TestProbe): Unit = {
    expectUsers(userManager, users.toSet)(me)
  }

  private def expectUsers(userManager: ActorRef, users: Set[UserInfo])(implicit me: TestProbe): Unit = {
    userManager ! RetrieveAllPlayers(me.ref)
    me expectMsg(Players(users))
  }

  private def expectNoUsers(userManager: ActorRef)(implicit me: TestProbe): Unit = expectUsers(userManager)(me)

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
