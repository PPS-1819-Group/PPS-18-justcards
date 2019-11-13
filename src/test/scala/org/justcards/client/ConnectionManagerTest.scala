package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.client.connection_manager.TcpConnectionManager
import org.justcards.commons.AppError._
import org.justcards.commons.{AppMessage, AvailableGames, AvailableLobbies, CreateLobby, ErrorOccurred, GameId, JoinLobby, LobbyCreated, LobbyJoined, LobbyUpdate, LogIn, Logged, RetrieveAvailableGames, RetrieveAvailableLobbies, UserId}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ConnectionManagerTest() extends TestKit(ActorSystem("ConnectionManagerTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  private val simpleServerAddress = new InetSocketAddress(Utils.serverHost,6701)
  private val unreachableServerAddress = new InetSocketAddress(Utils.serverHost,6702)
  private var serverSystem: ActorSystem = _
  private var connectionManager: ActorRef = _
  private var server: SenderServer = _

  override def beforeAll: Unit = {
    serverSystem = ActorSystem("server-system")
    serverSystem.actorOf(Server(simpleServerAddress, SimpleConnectionHandler(testActor, hasToSendRef = true)))
    val (cm, s) = initAndGetComponents(simpleServerAddress)
    connectionManager = cm
    server = s
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
      val appController = system.actorOf(TestAppController(testActor))
      serverSystem.actorOf(Server(unreachableServerAddress, SimpleConnectionHandler(testActor, hasToSendRef = true)))
      val connectionManager = system.actorOf(TcpConnectionManager(unreachableServerAddress)(appController))
      connectionManager ! InitializeConnection
      val server = waitToBeConnectedAndGetSenderServer()
      server kill()
      expectMsg(ErrorOccurred(CONNECTION_LOST))
    }

  }

  private def receiveMessageFromServerAndCheckItIsCorrectlyRedirectedToTheApplicationManager(message: AppMessage): Unit = {
    server send message
    expectMsg(message)
  }

  private def sendMessageToConnectionManagerAndCheckIfItIsCorrectlyRedirectedToTheServer(message: AppMessage): Unit = {
    connectionManager ! message
    expectMsg(message)
  }

  private def initAndGetComponents(serverAddress: InetSocketAddress): (ActorRef, SenderServer) = {
    val appController = system.actorOf(TestAppController(testActor))
    val connectionManager = system.actorOf(TcpConnectionManager(serverAddress)(appController))
    connectionManager ! InitializeConnection
    val server = waitToBeConnectedAndGetSenderServer()
    (connectionManager, server)
  }

  private def waitToBeConnectedAndGetSenderServer(): SenderServer = {
    receiveN(2).find(_.isInstanceOf[SenderServer]).get.asInstanceOf[SenderServer]
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
