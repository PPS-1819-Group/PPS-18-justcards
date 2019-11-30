package org.justcards.client.view

import akka.actor.{ActorRef, Props}
import org.justcards.commons.AppError._
import org.justcards.commons.{AppError, Card, GameId, LobbyId, TeamId, UserId}

trait View extends (ActorRef => Props)

object View {

  def apply(): View = ConsoleView()

  sealed trait ViewMessage
  case class ShowError(error: AppError.Value) extends ViewMessage
  case object ShowUsernameChoice extends ViewMessage
  case object ShowMenu extends ViewMessage
  case object MoveWasCorrect extends ViewMessage
  case object ShowTimeForMoveExceeded extends ViewMessage
  case class ShowLobbyCreation(games: Set[GameId]) extends ViewMessage
  case class ShowLobbies(lobbies: Set[(LobbyId, Set[UserId])]) extends ViewMessage
  case class ShowCreatedLobby(lobby: LobbyId) extends ViewMessage
  case class ShowJoinedLobby(lobby: LobbyId, members: Set[UserId]) extends ViewMessage
  case class ShowLobbyUpdate(lobby: LobbyId, members: Set[UserId]) extends ViewMessage
  case class ShowGameStarted(team: TeamId) extends ViewMessage
  case class ShowGameInformation(handCards: Set[Card], fieldCards: List[Card]) extends ViewMessage
  case class ViewChooseBriscola(availableBriscola: Set[String], timeout: Int) extends ViewMessage
  case class ShowTurn(handCards: Set[Card], fieldCards: List[Card], timeout: Int) extends ViewMessage
  case class ShowHandWinner(player: UserId) extends ViewMessage
  case class ShowMatchWinner(winnerTeam: TeamId, team1Points: Int, team2Points: Int) extends ViewMessage
  case class ShowGameWinner(team: TeamId) extends ViewMessage

  private[this] val HAND_WON_FINAL = "taken all the cards in the field!"

  val INPUT_SYMBOL: String = "> "
  val MENU_TITLE: String = "MENU"
  val ERROR_TITLE: String = "ERROR"
  val LOBBY_CREATION_TITLE: String = "LOBBY CREATION - Choose the game you want to play"
  val LOBBY_LIST_TITLE: String = "LOBBY LIST - Choose the lobby you want to join"
  val EXIT: String = "exit"
  val LOBBY_MESSAGE: String = "If you want to exit from the lobby, write " concat EXIT
  val HAND_WON: String = "You've " concat HAND_WON_FINAL
  val GAME_WON: String = "You've won the game!!!"
  val TIME_IS_UP: String = "Time'up! The game will decide for you"

  val LOBBY_CREATED_MESSAGE: LobbyId => String = "Your lobby has been created! ID = " + _.id
  val LOBBY_JOINED_MESSAGE: LobbyId => String = "You joined lobby " + _.id
  val GAME_STARTED: TeamId => String = "The game has started! You're on team " + _.name
  val HAND_LOST: UserId => String = _.name concat "has " concat HAND_WON_FINAL
  val GAME_LOST: TeamId => String = "You've lost! " concat _.name concat " wins the game"
  val MATCH_ENDS: (TeamId, (Int,Int)) => String = (winnerTeam, teamPoints) =>
    winnerTeam.name + " wins! " + teamPoints._1 + " - " + teamPoints._2
  val UNKNOWN_ERROR: AppError.Value => String = "Unknown error: " + _

  val ERROR_CONNECTION_LOST: String = "Error: Connection Lost"
  val ERROR_CANNOT_CONNECT: String = "Error: I can't connect"
  val ERROR_LAST_MESSAGE_LOST: String = "Error: Last message didn't arrive"

  val ERROR_USERNAME_ALREADY_USED: String = "Error: Username already used"
  val ERROR_USER_NOT_LOGGED: String = "Error: You're not logged"
  val ERROR_USER_ALREADY_LOGGED: String = "Error: You're already logged"
  val ERROR_USER_ALREADY_IN_LOBBY: String = "Error: You're already in a lobby"
  val ERROR_USER_WRONG_USERNAME: String = "Error: Action not allowed with this username"

  val ERROR_GAME_NOT_EXIST: String = "Error: Selected game doesn't exist"
  val ERROR_LOBBY_NOT_EXIST: String = "Error: Selected lobby doesn't exist"
  val ERROR_LOBBY_FULL: String = "Error: Selected lobby is already full"

  val ERROR_WRONG_CARD: String = "You can't play this card"
  val ERROR_WRONG_BRISCOLA: String = "This Briscola doesn't exist"

  val ERROR_WRONG_CHOICE: String = "Error: The selected choice is not available"

  def errorMessage(error: AppError.Value): String = error match {
    case CONNECTION_LOST => ERROR_CONNECTION_LOST
    case CANNOT_CONNECT => ERROR_CANNOT_CONNECT
    case MESSAGE_SENDING_FAILED => ERROR_LAST_MESSAGE_LOST
    case USER_ALREADY_PRESENT => ERROR_USERNAME_ALREADY_USED
    case USER_NOT_LOGGED => ERROR_USER_NOT_LOGGED
    case USER_ALREADY_LOGGED => ERROR_USER_ALREADY_LOGGED
    case USER_ALREADY_IN_A_LOBBY => ERROR_USER_ALREADY_IN_LOBBY
    case USER_WRONG_USERNAME => ERROR_USER_WRONG_USERNAME
    case GAME_NOT_EXISTING => ERROR_GAME_NOT_EXIST
    case LOBBY_NOT_EXISTING => ERROR_GAME_NOT_EXIST
    case LOBBY_FULL => ERROR_LOBBY_FULL
    case SELECTION_NOT_AVAILABLE => ERROR_WRONG_CHOICE
    case BRISCOLA_NOT_VALID =>ERROR_WRONG_BRISCOLA
    case CARD_NOT_VALID =>ERROR_WRONG_CARD
    case _ => UNKNOWN_ERROR(error)
  }
}
