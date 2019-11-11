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

  private val username = "username"
  private val game = GameId(1,"my-game")
  private val lobby = LobbyId(1)
  private val errorMessage = "error"

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

    "inform the user of the creation of a lobby when notified of its creation" in {
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

  private def initAndGetComponents:(UserCommandHandler, ActorRef) = {
    val appController = system.actorOf(AppController(ReSendConnectionManager(testActor),TestView(testActor, hasToSendRef = true)))
    val userCommandHandler = Utils.getRef[UserCommandHandler](receiveN)
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
}
