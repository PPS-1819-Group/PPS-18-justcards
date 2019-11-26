package org.justcards.client.controller

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.justcards.client.{TestConnectionManager, TestView}
import org.justcards.client.connection_manager.ConnectionManager.{Connected, DetailedErrorOccurred, InitializeConnection, TerminateConnection}
import org.justcards.client.controller.AppController._
import org.justcards.client.view.{MenuChoice, OptionConnectionFailed}
import org.justcards.client.view.View._
import org.justcards.commons.AppError._
import org.justcards.commons._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class AppControllerTest() extends WordSpecLike with Matchers with BeforeAndAfterAll {

  private implicit val system: ActorSystem = ActorSystem("AppControllerTest")

  override def afterAll: Unit = {
    system.terminate()
  }

  import org.justcards.client.Utils._

  "The application controller" when {

    "the application start" should {

      "tell the view to make the user choose a nickname" in {
        val (appController, testProbe) = initComponents
        appController ! Connected
        testProbe expectMsg ShowUsernameChoice
      }

    }

    "a user wants to log in" should {

      "send a LogIn message to the connection manager with the given username" in {
        val (appController, testProbe) = connect
        appController ! ChosenUsername(username)
        testProbe expectMsg LogIn(username)
      }

      "inform the user that was correctly logged in the system" in {
        val (appController, testProbe) = login(hasToBeLogged = false)
        appController ! Logged()
        testProbe expectMsg ShowMenu
      }

      "inform the user if another user with the same name is already present" in {
        val (appController, testProbe) = initComponents
        appController ! ErrorOccurred(USER_ALREADY_PRESENT)
        testProbe expectMsg ShowError(USER_ALREADY_PRESENT)
      }
    }

    "a user wants to the create a lobby" should {

      "send a message to the connection manager to get the available games" in {
        val (appController, testProbe) = login()
        appController ! MenuSelection(MenuChoice.CREATE_LOBBY)
        testProbe.expectMsgType[RetrieveAvailableGames]
      }

      "inform the user about the available games when received" in {
        val (appController, testProbe) = retrieveAvailableGames
        val games = Set(game)
        appController ! AvailableGames(games)
        testProbe expectMsg ShowLobbyCreation(games)
      }

      "send a message to the connection manager to create a lobby, when the user ask for it" in {
        val (appController, testProbe) = retrieveAvailableGames
        appController ! AppControllerCreateLobby(game)
        testProbe expectMsg CreateLobby(game)
      }

      "inform the user that he created a lobby when notified" in {
        val (appController, testProbe) = createLobby
        appController ! LobbyCreated(lobby)
        testProbe expectMsg ShowCreatedLobby(lobby)
      }

    }

    "a user wants to join a lobby" should {

      "send a message to the connection manager to get the available lobbies" in {
        val (appController, testProbe) = login()
        appController ! MenuSelection(MenuChoice.JOIN_LOBBY)
        testProbe.expectMsgType[RetrieveAvailableLobbies]
      }

      "inform the user about the available lobbies when received" in {
        val (appController, testProbe) = retrieveAvailableLobbies
        val lobbies = Set((lobby,Set(user)))
        appController ! AvailableLobbies(lobbies)
        testProbe expectMsg ShowLobbies(lobbies)
      }

      "send a message to the connection manager to join a lobby, when the user ask for it" in {
        val (appController, testProbe) = retrieveAvailableLobbies
        appController ! AppControllerJoinLobby(lobby)
        testProbe expectMsg JoinLobby(lobby)
      }

      "inform the user that he joined a lobby when notified" in {
        val (appController, testProbe) = joinLobby
        val tuple = (lobby, Set(user))
        appController ! LobbyJoined(tuple._1, tuple._2)
        testProbe expectMsg ShowJoinedLobby(tuple._1, tuple._2)
      }

    }

    "the user is in a lobby" should {

      "inform the user if notified of a update regarding the lobby he created" in {
        val (appController, testProbe) = createLobby
        appController ! LobbyCreated(lobby)
        testProbe receiveN 1
        val tuple = (lobby, Set(user))
        appController ! LobbyUpdate(tuple._1, tuple._2)
        testProbe expectMsg ShowLobbyUpdate(tuple._1, tuple._2)
      }

      "inform the user if notified of a update regarding the lobby he joined" in {
        val (appController, testProbe) = joinLobby
        val tuple = (lobby, Set(user))
        appController ! LobbyJoined(tuple._1, tuple._2)
        testProbe receiveN 1
        appController ! LobbyUpdate(tuple._1, tuple._2)
        testProbe expectMsg ShowLobbyUpdate(tuple._1, tuple._2)
      }

    }

    "a connection error occur" should {

      "inform the user that the system is not available and the application won't work" in {
        val (appController, testProbe) = initComponents
        appController ! ErrorOccurred(CANNOT_CONNECT)
        testProbe expectMsg ShowError(CANNOT_CONNECT)
      }

      "try to reconnect if the user asks to do it" in {
        val (appController, testProbe) = initComponents
        appController ! ErrorOccurred(CANNOT_CONNECT)
        testProbe receiveN 1
        appController ! ReconnectOption(OptionConnectionFailed.TRY_TO_RECONNECT)
        testProbe expectMsg InitializeConnection
      }

      "try to reconnect to the server and inform the user that the connection was lost" in {
        val (appController, testProbe) = initComponents
        appController ! ErrorOccurred(CONNECTION_LOST)
        testProbe expectMsgAllOf (ShowError(CONNECTION_LOST), InitializeConnection)
      }

      "if a message was not correctly delivered, without know the message, destroy the connection and notify the user of connection lost" in {
        val (appController, testProbe) = initComponents
        appController ! ErrorOccurred(MESSAGE_SENDING_FAILED)
        testProbe expectMsg TerminateConnection
      }

      "if a message was not correctly delivered, knowing the message, try to send the message another time" in {
        val (appController, testProbe) = initComponents
        val msg = LogIn(username)
        appController ! DetailedErrorOccurred(MESSAGE_SENDING_FAILED, msg)
        testProbe expectMsg msg
      }

    }

  }

  private def initComponents:(ActorRef, TestProbe) = {
    val testProbe = TestProbe()
    val testActor: ActorRef = testProbe.ref
    val appController = system.actorOf(AppController(TestConnectionManager(testActor),TestView(testActor)))
    testProbe receiveN 1
    (appController, testProbe)
  }

  private def connect: (ActorRef, TestProbe) = {
    val (appController, testProbe) = initComponents
    appController ! Connected
    testProbe receiveN 1
    (appController, testProbe)
  }

  private def login(hasToBeLogged: Boolean = true):(ActorRef, TestProbe) = {
    val (appController, testProbe) = connect
    appController ! ChosenUsername(username)
    if(hasToBeLogged) {
      appController ! Logged()
      testProbe receiveN 2
    } else
      testProbe receiveN 1
    (appController, testProbe)
  }

  private def retrieveAvailableGames:(ActorRef, TestProbe) = {
    val (appController, testProbe) = login()
    appController ! MenuSelection(MenuChoice.CREATE_LOBBY)
    testProbe receiveN 1
    (appController, testProbe)
  }

  private def createLobby:(ActorRef, TestProbe) = {
    val (appController, testProbe) = retrieveAvailableGames
    appController ! AppControllerCreateLobby(game)
    testProbe receiveN 1
    (appController, testProbe)
  }

  private def retrieveAvailableLobbies:(ActorRef, TestProbe) = {
    val (appController, testProbe) = login()
    appController ! MenuSelection(MenuChoice.JOIN_LOBBY)
    testProbe receiveN 1
    (appController, testProbe)
  }

  private def joinLobby: (ActorRef, TestProbe) = {
    val (appController, testProbe) = retrieveAvailableLobbies
    appController ! AppControllerJoinLobby(lobby)
    testProbe receiveN 1
    (appController, testProbe)
  }
}
