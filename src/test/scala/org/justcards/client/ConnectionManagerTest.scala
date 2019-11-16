package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.client.connection_manager.TcpConnectionManager
import org.justcards.commons.AppError._
import org.justcards.commons.{AppMessage, AvailableGames, AvailableLobbies, CreateLobby, ErrorOccurred, JoinLobby, LobbyCreated, LobbyJoined, LobbyUpdate, LogIn, Logged, RetrieveAvailableGames, RetrieveAvailableLobbies, UserId}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ConnectionManagerTest() extends TestKit(ActorSystem("ConnectionManagerTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  private val reachableServerAddress = new InetSocketAddress(Utils.serverHost,6701)
  private val unreachableServerAddress = new InetSocketAddress(Utils.serverHost,6702)
  private var serverSystem: ActorSystem = _

  override def beforeAll: Unit = {
    serverSystem = ActorSystem("server-system")
    serverSystem.actorOf(Server(reachableServerAddress, SimpleConnectionHandler(testActor)))
  }

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
      val appController = system.actorOf(TestAppController(testActor))
      val connectionManager = system.actorOf(TcpConnectionManager(unreachableServerAddress)(appController))
      connectionManager ! InitializeConnection
      expectMsg(ErrorOccurred(CANNOT_CONNECT))
    }

    "inform the application controller that the connection was lost" in {
      serverSystem.actorOf(Server(unreachableServerAddress, SimpleConnectionHandler(testActor)))
      val (_, server) = connectToServerAndGetComponents(unreachableServerAddress)
      server ! PoisonPill
      expectMsg(ErrorOccurred(CONNECTION_LOST))
    }

  }

  private def receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(message: AppMessage): Unit = {
    val (_, server) = connectToServerAndGetComponents(reachableServerAddress)
    server ! message
    expectMsg(message)
  }

  private def sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(message: AppMessage): Unit = {
    val (connectionManager, _) = connectToServerAndGetComponents(reachableServerAddress)
    connectionManager ! message
    expectMsg(message)
  }

  private def connectToServerAndGetComponents(serverAddress: InetSocketAddress): (ActorRef, ActorRef) = {
    val appController = system.actorOf(TestAppController(testActor))
    val connectionManager = system.actorOf(TcpConnectionManager(serverAddress)(appController))
    connectionManager ! InitializeConnection
    val server = waitToBeConnectedAndGetSenderServer()
    (connectionManager, server)
  }

  private def waitToBeConnectedAndGetSenderServer(): ActorRef = {
    receiveN(2).find(_.isInstanceOf[ActorRef]).get.asInstanceOf[ActorRef]
  }
}
