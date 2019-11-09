package org.justcards.server.user_manager

import scala.concurrent.duration._
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons._
import org.justcards.server.user_manager.UserManagerMessage.{Players, RetrieveAllPlayers, UserInfo, UserLogout}

class UserManagerTest extends TestKit(ActorSystem("ActorTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  import UserManagerTest._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "The user manager" when {

    val userManager = system.actorOf(UserManager())

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
        expectUsers(userManager, UserInfo(TEST_USERNAME, testActor))
      }

      "not register a user if the username is already present" in {
        userManager ! LogIn(TEST_USERNAME)
        expectMsgType[ErrorOccurred]
      }

      "unregister a user" in {
        userManager ! UserLogout(TEST_USERNAME, testActor)
        expectNoMessage(1 seconds)
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

    }

  }

  private def expectUsers(userManager: ActorRef, users: UserInfo*): Unit = {
    expectUsers(userManager, users.toSet)
  }

  private def expectUsers(userManager: ActorRef, users: Set[UserInfo]): Unit = {
    userManager ! RetrieveAllPlayers(testActor)
    expectMsg(Players(users.toSet))
  }

  private def expectNoUsers(userManager: ActorRef): Unit = expectUsers(userManager)

}

object UserManagerTest {

  val TEST_USERNAME: String = "test-username"
  val MULTI_TEST_USERNAME: String = "multi-test-username"
  val DOUBLE_USER_USERNAME: String = "double-user"

  def createActor(testActor: ActorRef, userManager: ActorRef): Props =
    Props(classOf[SimpleActor], testActor: ActorRef, userManager)

  private[this] class SimpleActor(testActor: ActorRef, userManager: ActorRef) extends Actor {
    override def receive: Receive = {
      case a: Logged => testActor ! a
      case msg => userManager ! msg
    }
  }

}
