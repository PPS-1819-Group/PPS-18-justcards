package org.justcards.client.connection_manager

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.justcards.client.Utils._
import org.justcards.client.connection_manager.ConnectionManager.{Connected, InitializeConnection}
import org.justcards.client.Server
import org.justcards.commons.AppError._
import org.justcards.commons._
import org.justcards.commons.actor_connection.Outer
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class ConnectionManagerTest extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private implicit val system: ActorSystem = ActorSystem("ConnectionManagerTest")
  private val serverAddress = new InetSocketAddress("localhost", 6789)
  private val serverAddress2 = new InetSocketAddress("localhost", 9876)
  private val unreachableServerAddress = new InetSocketAddress("localhost", 7896)

  override def beforeAll(): Unit = {
    val testProbe = TestProbe()
    val testActor: ActorRef = testProbe.ref
    system actorOf Server(serverAddress,testActor)
    testProbe receiveN 1
  }

  override def afterAll: Unit = {
    system terminate()
  }

  "The connection manager" should {

   "send a Logged message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(Logged())
    }

    "send an ErrorOccurred message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(ErrorOccurred(errorMessage))
    }

    "send an AvailableGames message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(AvailableGames(Set(game)))
    }

    "send an LobbyCreated message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(LobbyCreated(lobby))
    }

    "send an AvailableLobbies message to the application controller when received from the server" in {
      checkIfTheApplicationManagerReceiveTheMessage(AvailableLobbies(Set((lobby,Set(user)))))
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
      val connectionManager = system.actorOf(TcpConnectionManager(unreachableServerAddress)(testActor))
      connectionManager ! InitializeConnection
      testProbe expectMsg ErrorOccurred(CANNOT_CONNECT)
    }

    "inform the application controller that the connection was lost" in {
      val testProbe = TestProbe()
      val testActor: ActorRef = testProbe.ref
      system actorOf Server(serverAddress2, testActor, hasToDieAfterConnection = true)
      testProbe receiveN 1
      val connectionManager = system.actorOf(TcpConnectionManager(serverAddress2)(testActor))
      connectToServer(connectionManager, testProbe)
      testProbe expectMsg ErrorOccurred(CONNECTION_LOST)
    }

    "inform the application controller that the connection was established" in {
      val (connectionManager,testProbe) = initComponents
      connectionManager ! InitializeConnection
      testProbe expectMsg Connected
    }

  }

  private def checkIfTheApplicationManagerReceiveTheMessage(message: AppMessage): Unit = {
    val (connectionManager,testProbe) = initComponents
    connectToServer(connectionManager, testProbe)
    connectionManager ! Outer(message)
    testProbe expectMsg message
  }

  private def initComponents: (ActorRef, TestProbe) = {
    val testProbe = TestProbe()
    val testActor: ActorRef = testProbe.ref
    val connectionManager = system.actorOf(TcpConnectionManager(serverAddress)(testActor))
    (connectionManager, testProbe)
  }

  private def connectToServer(connectionManager: ActorRef, testProbe: TestProbe): Unit = {
    connectionManager ! InitializeConnection
    testProbe receiveN 1
  }
}
