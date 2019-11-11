package org.justcards.client.controller

import akka.actor.{Actor, Props, Stash}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.view.{View, ViewFactory}
import org.justcards.commons._
import org.justcards.commons.AppError._

trait AppController {
  def login(username: String): Unit
  def menuSelection(choice: String): Unit
  def createLobby(game: GameId): Unit
}

object MenuSelection {
  val createLobby = "create-lobby"
}

object AppController {
  def apply(connectionManager: ConnectionManager, viewFactory: ViewFactory) =
    Props(classOf[AppControllerActor], connectionManager, viewFactory)

  private[this] class AppControllerActor(connectionManager: ConnectionManager, viewFactory: ViewFactory) extends Actor
    with AppController {

    private val connectionManagerActor = context.actorOf(connectionManager(self))
    private val view: View = viewFactory(this)

    override def receive: Receive = default

    override def login(username: String): Unit = {
      changeContext(waitToBeLogged)
      connectionManagerActor ! LogIn(username)
    }

    override def menuSelection(choice: String): Unit = {
      choice match {
        case MenuSelection.createLobby =>
          changeContext(waitForAvailableGames)
          connectionManagerActor ! RetrieveAvailableGames()
        case _ => view error SELECTION_NOT_AVAILABLE
      }
    }

    override def createLobby(game: GameId): Unit = {
      changeContext(waitForLobbyCreation)
      connectionManagerActor ! CreateLobby(game)
    }

    private def waitToBeLogged: Receive = {
      case Logged(_) =>
        context become default
        view logged()
    }

    private def waitForAvailableGames: Receive = {
      case AvailableGames(games) =>
        context become default
        view showLobbyCreation games
    }

    private def waitForLobbyCreation: Receive = {
      case LobbyCreated(lobby) =>
        changeContext(inLobby)
        view lobbyCreated lobby
    }

    private def inLobby: Receive = {
      case _ => //TODO manage a user in a lobby
    }

    private def default: Receive = {
      case ErrorOccurred(message) => view error message
      case _ =>
    }

    private def changeContext(newBehaviour: Receive): Unit = {
      context become (newBehaviour orElse default)
    }

  }
}