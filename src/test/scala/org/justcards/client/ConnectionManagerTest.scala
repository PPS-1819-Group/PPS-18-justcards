package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.client.connection_manager.TcpConnectionManager
import org.justcards.commons.{ErrorOccurred, LogIn, Logged}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ConnectionManagerTest() extends TestKit(ActorSystem("ConnectionManagerTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  private val serverHost = "localhost"
  private val allowLoginAddress = new InetSocketAddress(serverHost,6701)
  private val denyLoginAddress = new InetSocketAddress(serverHost,6702)
  private var allowLoginSystem: ActorSystem = _
  private var denyLoginSystem: ActorSystem = _

  override def beforeAll: Unit = {
    allowLoginSystem = ActorSystem("allow-login-system")
    denyLoginSystem = ActorSystem("deny-login-system")

    allowLoginSystem.actorOf(Server(allowLoginAddress, AllowLoginManager))
    denyLoginSystem.actorOf(Server(denyLoginAddress, DenyLoginManager))
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
    TestKit.shutdownActorSystem(allowLoginSystem)
    TestKit.shutdownActorSystem(denyLoginSystem)
  }

  "The connection manager" should {
    "send a Logged message to the application controller if the user was allowed to log in" in {
      val username = "rigo"
      val appController = system.actorOf(TestAppController(testActor))
      val connectionManager = system.actorOf(TcpConnectionManager(allowLoginAddress)(appController))
      connectionManager ! LogIn(username)
      expectMsg(Logged(username))
    }

    "send an ErrorOccurred message to the application controller if the user was not allowed to log in" in {
      val username = "rigo"
      val appController = system.actorOf(TestAppController(testActor))
      val connectionManager = system.actorOf(TcpConnectionManager(denyLoginAddress)(appController))
      connectionManager ! LogIn(username)
      expectMsgType[ErrorOccurred]
    }
  }
}

object TestAppController {
  def apply(testActor: ActorRef) = Props(classOf[TestAppController], testActor)
  private[this] class TestAppController(testActor: ActorRef) extends Actor {
    override def receive: Receive = {
      case m => testActor ! m
    }
  }
}
