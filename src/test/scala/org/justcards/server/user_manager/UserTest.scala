package org.justcards.server.user_manager

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.TestProbe
import org.justcards.commons._
import org.justcards.commons.actor_connection.Outer

class UserTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  import UserTest._
  private implicit val system = ActorSystem("UserTest")

  override def afterAll: Unit = {
    system terminate()
  }

  "The UserActor" should {

    "redirect a message sent to him by the outer world to the userManager" in {
      val me = TestProbe()
      implicit val myRef = me.ref
      val user = createUser(myRef)
      val loginMessage = LogIn(TEST_USERNAME)
      user ! Outer(loginMessage)
      me expectMsg(Replicate(loginMessage))
    }

  }

  it when {

    "not logged" should {

      "not inform the userManager if the user logs out" in {
        val me = TestProbe()
        implicit val myRef = me.ref
        val user = createUser(myRef)
        user ! Outer(LogOut(TEST_USERNAME))
        me expectNoMessage()
      }

    }

    "logged" should {

      "inform the userManager if the user logs out" in {
        val me = TestProbe()
        implicit val myRef = me.ref
        val user = createUser(myRef)
        user ! Logged(TEST_USERNAME)
        me receiveN(1)
        user ! Outer(LogOut(TEST_USERNAME))
        me expectMsg(Replicate(LogOut(TEST_USERNAME)))
      }

      "not log out if the given username isn't the one previously given" in {
        val me = TestProbe()
        implicit val myRef = me.ref
        val user = createUser(myRef)
        user ! Logged(TEST_USERNAME)
        me receiveN(1)
        user ! Outer(LogOut(FAKE_USERNAME))
        val msgReceived = me receiveN(1)
        msgReceived should not be Replicate(LogOut(FAKE_USERNAME))
      }

      "not allow to log in twice" in {
        val me = TestProbe()
        implicit val myRef = me.ref
        val user = createUser(myRef)
        user ! Logged(TEST_USERNAME)
        me receiveN 1
        user ! Outer(LogIn(TEST_USERNAME))
        val msgReceived = me receiveN(1)
        msgReceived should not contain Replicate(LogIn(TEST_USERNAME))
      }

    }

    "in game" should {

      "redirect the messages received from the outer world to whom sent him the GameStarted message" in {
        val me = TestProbe()
        implicit val myRef = me.ref
        val consumer = system.actorOf(createConsumerActor)
        val user = createUser(consumer)
        user ! Logged(TEST_USERNAME)
        user ! LobbyJoined(LobbyId(1), Set())
        user ! GameStarted(List((UserId(1, TEST_USERNAME),TeamId(TEST_USERNAME))))
        user ! Outer(Briscola(TEST_USERNAME))
        me expectMsg Briscola(TEST_USERNAME)
      }

    }

  }

  private def createUser(myRef: ActorRef): ActorRef = {
    val userManagerStub = system.actorOf(createReplicateActor(myRef))
    system.actorOf(User(myRef, userManagerStub))
  }

}

object UserTest {

  val TEST_USERNAME: String = "test-username"
  val FAKE_USERNAME: String = "fake-username"
  case class Replicate(msg: Any)

  def createReplicateActor(ref: ActorRef): Props = Props(classOf[ReplicateMessageActor], ref)

  private[this] class ReplicateMessageActor(ref: ActorRef) extends Actor {
    override def receive: Receive = {
      case msg => ref ! Replicate(msg)
    }
  }

  def createConsumerActor: Props = Props(classOf[ConsumerMessageActor])

  private[this] class ConsumerMessageActor extends Actor {
    override def receive: Receive = {
      case _ =>
    }
  }
}
