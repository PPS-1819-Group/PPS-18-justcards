package org.justcards.client

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.TestProbe
import org.justcards.client.connection_manager.ConnectionManager.InitializeConnection
import org.justcards.client.connection_manager.TcpConnectionManager
import org.justcards.commons.AppError._
import org.justcards.commons._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import Utils._
import org.justcards.client.Server.ServerReady
import scala.concurrent.duration._

class ConnectionManagerTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private implicit val system: ActorSystem = ActorSystem("ConnectionManagerTest")
  private var nextAvailableServerPort = 6700

  override def afterAll: Unit = {
    system.terminate()
  }

  "The connection manager" should {

    "send a LogIn message to the server correctly when received from the application controller" in {
      checkIfTheServerReceiveTheMessage(LogIn(username))
    }

    "send a Logged message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(Logged())
    }

    "send an ErrorOccurred message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(ErrorOccurred(errorMessage))
    }

    "send a RetrieveAvailableGames message to the server correctly when received from the application controller" in {
      checkIfTheServerReceiveTheMessage(RetrieveAvailableGames())
    }

    "send an AvailableGames message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(AvailableGames(Set(game)))
    }

    "send a CreateLobby message to the server correctly when received from the application controller" in {
      checkIfTheServerReceiveTheMessage(CreateLobby(game))
    }

    "send an LobbyCreated message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(LobbyCreated(lobby))
    }

    "send a RetrieveAvailableLobbies message to the server correctly when received from the application controller" in {
      checkIfTheServerReceiveTheMessage(RetrieveAvailableLobbies())
    }

    "send an AvailableLobbies message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(AvailableLobbies(Set((lobby,Set(user)))))
    }

    "send a JoinLobby message to the server correctly when received from the application controller" in {
      checkIfTheServerReceiveTheMessage(JoinLobby(lobby))
    }

    "send an LobbyJoined message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(LobbyJoined(lobby,Set(user)))
    }

    "send an LobbyUpdate message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(LobbyUpdate(lobby,Set(user)))
    }

    "inform the application controller that the connection to the server cannot be established" in {
      val testProbe = TestProbe()
      val testActor: ActorRef = testProbe.ref
      val appController = system.actorOf(TestAppController(testActor))
      val connectionManager = system.actorOf(TcpConnectionManager(getNewServerAddress)(appController))
      connectionManager ! InitializeConnection
      testProbe expectMsg ErrorOccurred(CANNOT_CONNECT)
    }

    "inform the application controller that the connection was lost" in {
      val (connectionManager,testProbe) = initComponents
      val server = connectToServer(connectionManager, testProbe)
      server ! PoisonPill
      testProbe expectMsg ErrorOccurred(CONNECTION_LOST)
    }

  }

  private def checkIfTheApplicationManagerReceiveTheMessage(message: AppMessage): Unit = {
    val (connectionManager,testProbe) = initComponents
    val server = connectToServer(connectionManager, testProbe)
    server ! message
    testProbe expectMsg (5 seconds, message)
  }

  private def checkIfTheServerReceiveTheMessage(message: AppMessage): Unit = {
    val (connectionManager,testProbe) = initComponents
    connectToServer(connectionManager, testProbe)
    connectionManager ! message
    testProbe expectMsg message
  }

  private def initComponents: (ActorRef, TestProbe) = {
    val testProbe = TestProbe()
    val testActor: ActorRef = testProbe.ref
    val serverAddress = getNewServerAddress
    startServer(testProbe, testActor, serverAddress)
    val appController = system.actorOf(TestAppController(testActor))
    val connectionManager = system.actorOf(TcpConnectionManager(serverAddress)(appController))
    (connectionManager, testProbe)
  }

  private def startServer(testProbe: TestProbe, testActor: ActorRef, serverAddress: InetSocketAddress): Unit = {
    system.actorOf(Server(serverAddress, SimpleConnectionHandler(testActor), testActor))
    testProbe expectMsg ServerReady
  }

  private def connectToServer(connectionManager: ActorRef, testProbe: TestProbe): ActorRef = {
    connectionManager ! InitializeConnection
    retrieveServerRef(testProbe)
  }

  private def retrieveServerRef(testProbe: TestProbe): ActorRef = {
    testProbe.receiveN(2).find(_.isInstanceOf[ActorRef]).get.asInstanceOf[ActorRef]
  }

  private def getNewServerAddress: InetSocketAddress = synchronized {
    val serverAddress = new InetSocketAddress(serverHost,nextAvailableServerPort)
    nextAvailableServerPort += 1
    serverAddress
  }
}
