package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Terminated}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons._
import org.justcards.commons.actor_connection.Outer

class UserTest extends TestKit(ActorSystem("ActorTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  import UserTest._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "The UserActor" should {

    val userManagerStub = system.actorOf(createReplicateActor(testActor))
    val user = system.actorOf(User(testActor, userManagerStub))

    "redirect a message sent to him by the outer world to the userManager" in {
      val loginMessage = LogIn(TEST_USERNAME)
      user ! Outer(loginMessage)
      expectMsg(Replicate(loginMessage))
    }

  }

  it when {

    val userManagerStub = system.actorOf(createReplicateActor(testActor))

    "not logged" should {

      val user = system.actorOf(User(testActor, userManagerStub))

      "not inform the userManager if the user logs out" in {
        user ! Outer(LogOut(TEST_USERNAME))
        expectNoMessage()
      }

    }

    "logged" should {

      "inform the userManager if the user logs out" in {
        val user = system.actorOf(User(testActor, userManagerStub))
        user ! Logged(TEST_USERNAME)
        user ! Outer(LogOut(TEST_USERNAME))
        expectMsg(Replicate(LogOut(TEST_USERNAME)))
      }

    }

  }

}

object UserTest {

  val TEST_USERNAME: String = "test-username"
  case class Replicate(msg: Any)

  def createReplicateActor(testActor: ActorRef): Props = Props(classOf[ReplicateMessageActor], testActor)

  private[this] class ReplicateMessageActor(testActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case msg => testActor ! Replicate(msg)
    }
  }

}
