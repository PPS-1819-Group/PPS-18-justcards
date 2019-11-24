package org.justcards.client.controller

import akka.actor.{Actor, ActorContext, Props}
import org.justcards.commons._
import org.justcards.commons.AppError._
import org.justcards.client.connection_manager.ConnectionManager
import org.justcards.client.connection_manager.ConnectionManager.{Connected, DetailedErrorOccurred, InitializeConnection, TerminateConnection}
import org.justcards.client.view.{MenuChoice, OptionConnectionFailed, View}
import org.justcards.client.view.View._
import org.justcards.client.view.MenuChoice._
import org.justcards.client.view.OptionConnectionFailed._

object AppController {

  def apply(connectionManager: ConnectionManager, view: View) =
    Props(classOf[AppControllerActor], connectionManager, view)

  case class ChosenUsername(username: String)
  case class MenuSelection(choice: MenuChoice.Value)
  case class AppControllerCreateLobby(game: GameId)
  case class AppControllerJoinLobby(lobby: LobbyId)
  case object ExitFromLobby

  private[this] class AppControllerActor(connectionManager: ConnectionManager, view: View) extends Actor {

    private val connectionManagerActor = context.actorOf(connectionManager(self))
    private val viewActor = context.actorOf(view(self))
    connectionManagerActor ! InitializeConnection

    override def receive: Receive = waitToBeConnected orElse default

    /*override def reconnectOrExit(choice: OptionConnectionFailed.Value): Unit = ???*/


    private def waitToBeConnected: Receive = {
      case Connected =>
        context >>> connected
        viewActor ! ShowUsernameChoice
      case ErrorOccurred(m) if m == CANNOT_CONNECT.toString =>
        viewActor ! ShowError(CANNOT_CONNECT)
    }

    private def connected: Receive = {
      case ChosenUsername(username) =>
        context >>> waitToBeLogged
        connectionManagerActor ! LogIn(username)
    }

    private def waitToBeLogged: Receive = {
      case Logged(_) =>
        context >>> logged
        viewActor ! ShowMenu
    }

    private def logged: Receive = {
      case MenuSelection(choice) => choice match {
        case CREATE_LOBBY =>
          context >>> lobbyCreation
          connectionManagerActor ! RetrieveAvailableGames()
        case JOIN_LOBBY =>
          context >>> searchForLobby
          connectionManagerActor ! RetrieveAvailableLobbies()
        case _ => viewActor ! ShowError(SELECTION_NOT_AVAILABLE)
      }
    }

    private def lobbyCreation: Receive = {
      case AvailableGames(games) => viewActor ! ShowLobbyCreation(games)
      case AppControllerCreateLobby(game) =>
        context >>> waitForLobbyCreation
        connectionManagerActor ! CreateLobby(game)
    }

    private def waitForLobbyCreation: Receive = {
      case LobbyCreated(lobby) =>
        context >>> inLobby(lobby)
        viewActor ! ShowCreatedLobby(lobby)
    }

    private def searchForLobby: Receive = {
      case AvailableLobbies(lobbies) =>
        viewActor ! ShowLobbies(lobbies)
      case AppControllerJoinLobby(lobby) =>
        context >>> waitForLobbyJoin
        connectionManagerActor ! JoinLobby(lobby)
    }

    private def waitForLobbyJoin: Receive = {
      case LobbyJoined(lobby, members) =>
        context >>> inLobby(lobby)
        viewActor ! ShowJoinedLobby(lobby,members)
    }

    private def inLobby(myLobby: LobbyId): Receive = {
      case LobbyUpdate(lobby, members) => viewActor ! ShowLobbyUpdate(lobby,members)
      case ExitFromLobby => connectionManagerActor ! OutOfLobby(myLobby)
    }

    private def default: Receive = {
      case ErrorOccurred(message) =>
        val error = AppError.values.find(_.toString == message)
        if (error.isDefined) error.get match {
          case CONNECTION_LOST => connectionLost()
          case MESSAGE_SENDING_FAILED => connectionManagerActor ! TerminateConnection
          case USER_ALREADY_PRESENT => chooseNicknameAgain(USER_ALREADY_PRESENT)
          case USER_NOT_LOGGED => chooseNicknameAgain(USER_NOT_LOGGED)
          case m => notifyError(m)
        }
      case DetailedErrorOccurred(MESSAGE_SENDING_FAILED, message) =>
        connectionManagerActor ! message
    }

    private def connectionLost(): Unit = {
      context >>> waitToBeConnected
      viewActor ! ShowError(CONNECTION_LOST)
      connectionManagerActor ! InitializeConnection
    }

    private def notifyError(message: AppError.Value): Unit = viewActor ! ShowError(message)

    private def chooseNicknameAgain(error: AppError.Value): Unit = {
      viewActor ! ShowError(error)
      viewActor ! ShowUsernameChoice
    }

    private implicit class RichContext(context: ActorContext) {
      def >>>(behaviour: Receive): Unit = {
        require(behaviour != default)
        context become (behaviour orElse default)
      }
    }
  }
}