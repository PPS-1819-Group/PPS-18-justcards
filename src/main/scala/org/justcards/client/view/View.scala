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
  case object ShowTimeForMoveExceeded extends ViewMessage
  case class ShowLobbyCreation(games: Set[GameId]) extends ViewMessage
  case class ShowLobbies(lobbies: Set[(LobbyId, Set[UserId])]) extends ViewMessage
  case class ShowCreatedLobby(lobby: LobbyId) extends ViewMessage
  case class ShowJoinedLobby(lobby: LobbyId, members: Set[UserId]) extends ViewMessage
  case class ShowLobbyUpdate(lobby: LobbyId, members: Set[UserId]) extends ViewMessage
  case class ShowGameStarted(team: TeamId) extends ViewMessage
  case class ShowGameInformation(handCards: Set[Card], fieldCards: List[Card]) extends ViewMessage
  case class ViewChooseBriscola(timeout: Int) extends ViewMessage
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
  val CHOOSE_NICKNAME: String = "Choose your nickname:"
  val HAND_WON: String = "You've " concat HAND_WON_FINAL
  val GAME_WON: String = "You've won the game!!!"
  val EXIT: String = "exit"
  val LOBBY_MESSAGE: String = "If you want to exit from the lobby, write " concat EXIT

  def LOBBY_CREATED_MESSAGE(lobby: LobbyId): String = "Your lobby has been created! ID = " + lobby.id
  def LOBBY_JOINED_MESSAGE(lobby: LobbyId): String = "You joined lobby " + lobby.id
  def GAME_STARTED(team: TeamId): String = "The game has started! You're on team " + team.name
  def HAND_LOST(player: UserId): String = player.name concat "has " concat HAND_WON_FINAL
  def GAME_LOST(team: TeamId): String = "You've lost! " concat team.name concat " wins the game"
  def MATCH_ENDS(winnerTeam: TeamId, teamPoints: (Int,Int)): String =
    winnerTeam.name + " wins! " + teamPoints._1 + " - " + teamPoints._2
  def UNKNOWN_ERROR(error: AppError.Value): String = "Unknown error: " + error

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
    case _ => UNKNOWN_ERROR(error)
  }
}
