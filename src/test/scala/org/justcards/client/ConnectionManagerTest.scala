package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.{TestKit, TestProbe}
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.client.connection_manager.TcpConnectionManager
import org.justcards.commons.AppError._
import org.justcards.commons._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ConnectionManagerTest() extends WordSpecLike
  with Matchers with BeforeAndAfterAll {

  private implicit val system: ActorSystem = ActorSystem("ConnectionManagerTest")

  private var nextAvailableServerPort = 6700
  private val serverSystem: ActorSystem = ActorSystem("server-system")

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
    TestKit.shutdownActorSystem(serverSystem)
  }

  "The connection manager" should {

    "send a LogIn message to the server correctly when received from the application controller" in {
      sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(LogIn(Utils.username))
    }

    "send a Logged message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(Logged())
    }

    "send an ErrorOccurred message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(ErrorOccurred(Utils.errorMessage))
    }

    "send a RetrieveAvailableGames message to the server correctly when received from the application controller" in {
      sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(RetrieveAvailableGames())
    }

    "send an AvailableGames message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(AvailableGames(Set(Utils.game)))
    }

    "send a CreateLobby message to the server correctly when received from the application controller" in {
      sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(CreateLobby(Utils.game))
    }

    "send an LobbyCreated message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(LobbyCreated(Utils.lobby))
    }

    "send a RetrieveAvailableLobbies message to the server correctly when received from the application controller" in {
      sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(RetrieveAvailableLobbies())
    }

    "send an AvailableLobbies message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(AvailableLobbies(Set((Utils.lobby,Set(Utils.user)))))
    }

    "send a JoinLobby message to the server correctly when received from the application controller" in {
      sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(JoinLobby(Utils.lobby))
    }

    "send an LobbyJoined message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(LobbyJoined(Utils.lobby,Set(Utils.user)))
    }

    "send an LobbyUpdate message to the application controller when received from the server" in {
      receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(LobbyUpdate(Utils.lobby,Set(Utils.user)))
    }

    "inform the application controller that the connection to the server cannot be established" in {
      val testProbe = TestProbe()
      implicit val testActor: ActorRef = testProbe.ref
      val appController = system.actorOf(TestAppController(testActor))
      val connectionManager = system.actorOf(TcpConnectionManager(getNewServerAddress)(appController))
      connectionManager ! InitializeConnection
      testProbe.expectMsg(ErrorOccurred(CANNOT_CONNECT))
    }

    "inform the application controller that the connection was lost" in {
      val (_, server,testProbe) = connectToServerAndGetComponents
      server ! PoisonPill
      testProbe.expectMsg(ErrorOccurred(CONNECTION_LOST))
    }

  }

  private def receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(message: AppMessage): Unit = {
    val (_, server,testProbe) = connectToServerAndGetComponents
    server ! message
    testProbe.expectMsg(message)
  }

  private def sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(message: AppMessage): Unit = {
    val (connectionManager, _,testProbe) = connectToServerAndGetComponents
    connectionManager ! message
    testProbe.expectMsg(message)
  }

  private def connectToServerAndGetComponents: (ActorRef, ActorRef, TestProbe) = {
    val testProbe = TestProbe()
    implicit val testActor: ActorRef = testProbe.ref
    val serverAddress = getNewServerAddress
    serverSystem.actorOf(Server(serverAddress, SimpleConnectionHandler(testActor)))
    val appController = system.actorOf(TestAppController(testActor))
    val connectionManager = system.actorOf(TcpConnectionManager(serverAddress)(appController))
    connectionManager ! InitializeConnection
    val server = waitToBeConnectedAndGetSenderServer(testProbe)
    (connectionManager, server, testProbe)
  }

  private def waitToBeConnectedAndGetSenderServer(testActor: TestProbe): ActorRef = {
    testActor.receiveN(2).find(_.isInstanceOf[ActorRef]).get.asInstanceOf[ActorRef]
  }

  private def getNewServerAddress: InetSocketAddress = synchronized {
    val serverAddress = new InetSocketAddress(Utils.serverHost,nextAvailableServerPort)
    nextAvailableServerPort += 1
    serverAddress
  }
}
