package org.justcards.client

import akka.actor.{ActorRef, ActorSystem}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.TestProbe
import org.justcards.client.TestView.ChooseNickname
import org.justcards.client.connection_manager.ConnectionManager.{Connected, InitializeConnection}
import org.justcards.client.controller.AppController
import org.justcards.client.view.MenuChoice
import org.justcards.commons._
import org.justcards.commons.AppError._

class AppControllerTest() extends WordSpecLike
  with Matchers with BeforeAndAfterAll {

  private implicit val system: ActorSystem = ActorSystem("ConnectionManagerTest")

  override def afterAll: Unit = {
    system.terminate()
  }

  import Utils._

  "The application controller" should {
    "tell the view to make the user choose a nickname when the application start" in {
      connect
    }
  }

  "When a user wants to log in the application controller" should {

    "send a LogIn message to the connection manager with the given username" in {
      login
    }

    "inform the user that was correctly logged in the system" in {
      val (_,appController, testProbe) = login
      val msg = Logged()
      appController ! msg
      testProbe expectMsg msg
    }

    "inform the user if an error occurred" in {
      val (_,appController, testProbe) = initAndGetComponents
      val error = ErrorOccurred(errorMessage)
      appController ! error
      testProbe expectMsg error
    }
  }

  "During the creation of a lobby the application manager" should {

    "send a message to the connection manager to get the available games" in {
      retrieveAvailableGames
    }

    "inform the user about the available games when received" in {
      val (_,appController, testProbe) = retrieveAvailableGames
      val msg = AvailableGames(Set(game))
      appController ! msg
      testProbe expectMsg msg
    }

    "send a message to the connection manager to create a lobby, when the user ask for it" in {
      createLobby
    }

    "inform the user that he created a lobby when notified" in {
      val (_,appController, testProbe) = createLobby
      val msg = LobbyCreated(lobby)
      appController ! msg
      testProbe expectMsg msg
    }

  }

  "During the joining of a lobby the application manager" should {

    "send a message to the connection manager to get the available lobbies" in {
      retrieveAvailableLobbies
    }

    "inform the user about the available lobbies when received" in {
      val (_,appController, testProbe) = retrieveAvailableLobbies
      val msg = AvailableLobbies(Set((lobby,Set(user))))
      appController ! msg
      testProbe expectMsg msg
    }

    "send a message to the connection manager to join a lobby, when the user ask for it" in {
      joinLobby
    }

    "inform the user that he joined a lobby when notified" in {
      val (_,appController, testProbe) = joinLobby
      val msg = LobbyJoined(lobby, Set(user))
      appController ! msg
      testProbe expectMsg msg
    }

  }

  "When the user is in a lobby the application controller" should {

    "inform the user if notified of a update regarding the lobby he created" in {
      val (_,appController, testProbe) = createLobby
      appController ! LobbyCreated(lobby)
      testProbe.receiveN(1)
      val msg = LobbyUpdate(lobby,Set(user))
      appController ! msg
      testProbe expectMsg msg
    }

    "inform the user if notified of a update regarding the lobby he joined" in {
      val (_,appController, testProbe) = joinLobby
      appController ! LobbyJoined(lobby,Set(user))
      testProbe.receiveN(1)
      val msg = LobbyUpdate(lobby,Set(user))
      appController ! msg
      testProbe expectMsg msg
    }

  }

  "When there is a connection error the application controller" should {

    "inform the user that the system is not available and the application won't work" in {
      val (_, appController, testProbe) = initAndGetComponents
      val msg = ErrorOccurred(CANNOT_CONNECT)
      appController ! msg
      testProbe expectMsg msg
    }

    "try to reconnect to the server and inform the user that the connection was lost" in {
      val (_, appController, testProbe) = initAndGetComponents
      val msg = ErrorOccurred(CONNECTION_LOST)
      appController ! msg
      testProbe expectMsgAllOf (msg, InitializeConnection)
    }

    "inform the user if a message was not correctly delivered" in {
      val (_, appController, testProbe) = initAndGetComponents
      val msg = ErrorOccurred(MESSAGE_SENDING_FAILED)
      appController ! msg
      testProbe expectMsg msg
    }

  }

  private def initAndGetComponents:(UserCommandHandler, ActorRef, TestProbe) = {
    val testProbe = TestProbe()
    implicit val testActor: ActorRef = testProbe.ref
    val appController = system.actorOf(AppController(ReSendConnectionManager(testActor),TestView(testActor, hasToSendRef = true)))
    val userCommandHandler = getRef[UserCommandHandler](testProbe.receiveN)
    testProbe expectMsg InitializeConnection
    (userCommandHandler, appController, testProbe)
  }

  private def connect: (UserCommandHandler, ActorRef, TestProbe) = {
    val (userCommandHandler, appController, testProbe) = initAndGetComponents
    appController ! Connected
    testProbe expectMsg ChooseNickname
    (userCommandHandler, appController, testProbe)
  }

  private def login:(UserCommandHandler, ActorRef, TestProbe) = {
    val (userCommandHandler, appController, testProbe) = connect
    userCommandHandler login username
    testProbe.expectMsg(LogIn(username))
    (userCommandHandler, appController, testProbe)
  }

  private def retrieveAvailableGames:(UserCommandHandler, ActorRef, TestProbe) = {
    val (userCommandHandler, appController, testProbe) = login
    userCommandHandler menuSelection MenuChoice.createLobby
    testProbe.expectMsgType[RetrieveAvailableGames]
    (userCommandHandler, appController, testProbe)
  }

  private def createLobby:(UserCommandHandler, ActorRef, TestProbe) = {
    val (userCommandHandler, appController, testProbe) = retrieveAvailableGames
    userCommandHandler createLobby game
    testProbe.expectMsg(CreateLobby(game))
    (userCommandHandler, appController, testProbe)
  }

  private def retrieveAvailableLobbies:(UserCommandHandler, ActorRef, TestProbe) = {
    val (userCommandHandler, appController, testProbe) = login
    userCommandHandler menuSelection MenuChoice.joinLobby
    testProbe.expectMsgType[RetrieveAvailableLobbies]
    (userCommandHandler, appController, testProbe)
  }

  private def joinLobby: (UserCommandHandler, ActorRef, TestProbe) = {
    val (userCommandHandler, appController, testProbe) = retrieveAvailableLobbies
    userCommandHandler joinLobby Utils.lobby
    testProbe.expectMsg(JoinLobby(lobby))
    (userCommandHandler, appController, testProbe)
  }
}
