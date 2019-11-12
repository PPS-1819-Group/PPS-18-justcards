package org.justcards.client

import akka.actor.{ActorRef, ActorSystem}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.testkit.{ImplicitSender, TestKit}
import org.justcards.client.controller.{AppController, MenuSelection}
import org.justcards.commons._

class AppControllerTest() extends TestKit(ActorSystem("AppControllerTest")) with ImplicitSender with WordSpecLike
  with Matchers with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import Utils._

  "When a user wants to log in the application controller" should {

    "send a LogIn message to the connection manager with the given username" in {
      login
    }

    "inform the user that was correctly logged in the system" in {
      val (_,appController) = login
      appController ! Logged()
      expectMsgType[Logged]
    }

    "inform the user if an error occurred" in {
      val (_,appController) = initAndGetComponents
      val error = ErrorOccurred(errorMessage)
      appController ! error
      expectMsg(error)
    }
  }

  "During the creation of a lobby the application manager" should {

    "send a message to the connection manager to get the available games" in {
      retrieveAvailableGames
    }

    "inform the user about the available games when received" in {
      val (_,appController) = retrieveAvailableGames
      val msg = AvailableGames(Set(game))
      appController ! msg
      expectMsg(msg)
    }

    "send a message to the connection manager to create a lobby, when the user ask for it" in {
      createLobby
    }

    "inform the user that he created a lobby when notified" in {
      val (_,appController) = createLobby
      val msg = LobbyCreated(lobby)
      appController ! msg
      expectMsg(msg)
    }

    "inform the user if an error occurred" in {
      val error = ErrorOccurred(errorMessage)
      val (_,appController) = retrieveAvailableGames
      appController ! error
      expectMsg(error)
    }

  }

  "During the joining of a lobby the application manager" should {

    "send a message to the connection manager to get the available lobbies" in {
      retrieveAvailableLobbies
    }

    "inform the user about the available lobbies when received" in {
      val (_,appController) = retrieveAvailableLobbies
      val msg = AvailableLobbies(Set(lobby))
      appController ! msg
      expectMsg(msg)
    }

    "send a message to the connection manager to join a lobby, when the user ask for it" in {
      joinLobby
    }

    "inform the user that he joined a lobby when notified" in {
      val (_,appController) = joinLobby
      val msg = LobbyJoined(lobby, Set(user))
      appController ! msg
      expectMsg(msg)
    }

    "inform the user if an error occurred" in {
      val error = ErrorOccurred(errorMessage)
      val (_,appController) = retrieveAvailableLobbies
      appController ! error
      expectMsg(error)
    }

  }

  "When the user is in a lobby the application controller" should {

    "inform the user if notified of a update regarding the lobby he created" in {
      val (_,appController) = createLobby
      appController ! LobbyCreated(lobby)
      receiveN(1)
      val msg = LobbyUpdate(lobby,Set(user))
      appController ! msg
      expectMsg(msg)
    }

    "inform the user if notified of a update regarding the lobby he joined" in {
      val (_,appController) = joinLobby
      appController ! LobbyJoined(lobby,Set(user))
      receiveN(1)
      val msg = LobbyUpdate(lobby,Set(user))
      appController ! msg
      expectMsg(msg)
    }

  }

  private def initAndGetComponents:(UserCommandHandler, ActorRef) = {
    val appController = system.actorOf(AppController(ReSendConnectionManager(testActor),TestView(testActor, hasToSendRef = true)))
    val userCommandHandler = getRef[UserCommandHandler](receiveN)
    (userCommandHandler, appController)
  }

  private def login:(UserCommandHandler, ActorRef) = {
    val (userCommandHandler, appController) = initAndGetComponents
    userCommandHandler login username
    expectMsg(LogIn(username))
    (userCommandHandler, appController)
  }

  private def retrieveAvailableGames:(UserCommandHandler, ActorRef) = {
    val (userCommandHandler,appController) = login
    userCommandHandler menuSelection MenuSelection.createLobby
    expectMsgType[RetrieveAvailableGames]
    (userCommandHandler,appController)
  }

  private def createLobby:(UserCommandHandler, ActorRef) = {
    val (userCommandHandler,appController) = retrieveAvailableGames
    userCommandHandler createLobby game
    expectMsg(CreateLobby(game))
    (userCommandHandler,appController)
  }

  private def retrieveAvailableLobbies:(UserCommandHandler, ActorRef) = {
    val (userCommandHandler,appController) = login
    userCommandHandler menuSelection MenuSelection.joinLobby
    expectMsgType[RetrieveAvailableLobbies]
    (userCommandHandler,appController)
  }

  private def joinLobby: (UserCommandHandler, ActorRef) = {
    val (userCommandHandler,appController) = retrieveAvailableLobbies
    userCommandHandler joinLobby Utils.lobby
    expectMsg(JoinLobby(lobby))
    (userCommandHandler,appController)
  }
}
