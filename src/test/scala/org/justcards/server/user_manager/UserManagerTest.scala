package org.justcards.server.user_manager

import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.commons._
import org.justcards.server.user_manager.UserManagerMessage.{Players, RetrieveAllPlayers, UserInfo}

class UserManagerTest() extends TestKit(ActorSystem("ActorTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "The user manager" when {

    val userManager = system.actorOf(UserManager())

    "created" should {

      "not contain any logged user" in {
        userManager ! RetrieveAllPlayers(testActor)
        expectMsg(Players(Set()))
      }

    }

    "is at runtime" should {

      "register a user" in {
        userManager ! LogIn("test-username")
        expectMsgType[Logged]
      }

      "save the user after its registration" in {
        userManager ! RetrieveAllPlayers(testActor)
        expectMsg(Players(Set(UserInfo("test-username", testActor))))
      }

      "not register a user if the username is already present" in {
        userManager ! LogIn("test-username")
        expectMsgType[ErrorOccurred]
      }

    }

  }
}
