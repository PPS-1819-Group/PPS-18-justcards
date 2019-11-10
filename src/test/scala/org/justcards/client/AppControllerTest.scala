package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.client.controller.AppController
import org.justcards.commons._

class AppControllerTest() extends TestKit(ActorSystem("AppControllerTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "The application controller" should {

    "send a LogIn message to the connection manager with the given username" in {
      val username = "username"
      system.actorOf(AppController(TestConnectionManager(testActor),LogInView(username, testActor)))
      expectMsg(LogIn(username))
    }

    "inform the user that he was correctly logged in the system" in {
      val appController = system.actorOf(AppController(TestConnectionManager(testActor),TestView(testActor)))
      appController ! Logged()
      expectMsgType[Logged]
    }

    "inform the user if an error occurred" in {
      val error = ErrorOccurred("error")
      val appController = system.actorOf(AppController(TestConnectionManager(testActor),TestView(testActor)))
      appController ! error
      expectMsg(error)
    }
  }
}
