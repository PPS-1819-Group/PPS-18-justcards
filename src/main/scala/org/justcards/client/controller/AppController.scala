package org.justcards.client.controller

import akka.actor.{Actor, Props}
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.connection_manager.ConnectionManager.{Connected, DetailedErrorOccurred, InitializeConnection, TerminateConnection}
import org.justcards.client.view.{MenuChoice, View, ViewFactory}
import org.justcards.commons._
import org.justcards.commons.AppError._

trait AppController {
  def login(username: String): Unit
  def menuSelection(choice: MenuChoice.Value): Unit
  def createLobby(game: GameId): Unit
  def joinLobby(lobby: LobbyId): Unit
}

object AppController {

  def apply(connectionManager: ConnectionManager, viewFactory: ViewFactory) =
    Props(classOf[AppControllerActor], connectionManager, viewFactory)

  private[this] class AppControllerActor(connectionManager: ConnectionManager, viewFactory: ViewFactory) extends Actor
    with AppController {

    private val connectionManagerActor = context.actorOf(connectionManager(self))
    private val view: View = viewFactory(this)
    connectionManagerActor ! InitializeConnection

    override def receive: Receive = waitToBeConnected orElse default

    override def login(username: String): Unit = {
      changeContext(waitToBeLogged)
      connectionManagerActor ! LogIn(username)
    }

    override def menuSelection(choice: MenuChoice.Value): Unit = {
      choice match {
        case MenuChoice.createLobby =>
          changeContext(waitForAvailableGames)
          connectionManagerActor ! RetrieveAvailableGames()
        case MenuChoice.joinLobby =>
          changeContext(waitForAvailableLobbies)
          connectionManagerActor ! RetrieveAvailableLobbies()
        case _ => view error SELECTION_NOT_AVAILABLE
      }
    }

    override def createLobby(game: GameId): Unit = {
      changeContext(waitForLobbyCreation)
      connectionManagerActor ! CreateLobby(game)
    }

    override def joinLobby(lobby: LobbyId): Unit = {
      changeContext(waitForLobbyJoin)
      connectionManagerActor ! JoinLobby(lobby)
    }

    private def waitToBeConnected: Receive = {
      case Connected =>
        context become default
        view chooseNickname()
      case ErrorOccurred(m) if m == CANNOT_CONNECT.toString =>
        view error CANNOT_CONNECT
    }

    private def waitToBeLogged: Receive = {
      case Logged(_) =>
        context become default
        view showMenu()
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

    private def waitForAvailableLobbies: Receive = {
      case AvailableLobbies(lobbies) =>
        context become default
        view showLobbyJoin lobbies
    }

    private def waitForLobbyJoin: Receive = {
      case LobbyJoined(lobby, members) =>
        changeContext(inLobby)
        view lobbyJoined (lobby,members)
    }

    private def inLobby: Receive = {
      case LobbyUpdate(lobby, members) => view lobbyUpdate (lobby,members)
    }

    private def default: Receive = {
      case ErrorOccurred(message) =>
        val error = AppError.values.find(_.toString == message)
        if (error.isDefined) error get match {
          case CONNECTION_LOST => connectionLost()
          case MESSAGE_SENDING_FAILED => connectionManagerActor ! TerminateConnection
          case USER_ALREADY_PRESENT | USER_NOT_LOGGED =>
            notifyErrorAndChangeBehaviourToDefault(error.get)
            view chooseNickname()
          case GAME_NOT_EXISTING => notifyErrorAndChangeBehaviourToDefault(GAME_NOT_EXISTING)
          case LOBBY_NOT_EXISTING => notifyErrorAndChangeBehaviourToDefault(LOBBY_NOT_EXISTING)
          case LOBBY_FULL => notifyErrorAndChangeBehaviourToDefault(LOBBY_FULL)
          case m => view error m
        }
      case DetailedErrorOccurred(MESSAGE_SENDING_FAILED, message) =>
        connectionManagerActor ! message
      case _ =>
    }

    private def connectionLost(): Unit = {
      changeContext(waitToBeConnected)
      view error CONNECTION_LOST
      connectionManagerActor ! InitializeConnection
    }

    private def changeContext(newBehaviour: Receive): Unit = {
      context become (newBehaviour orElse default)
    }

    private def notifyErrorAndChangeBehaviourToDefault(message: AppError.Value): Unit = {
      defaultBehaviour()
      view error message
    }

    private def defaultBehaviour(): Unit = context become default
  }
}