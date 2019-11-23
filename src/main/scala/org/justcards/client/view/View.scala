package org.justcards.client.view

import org.justcards.client.controller.AppController
import org.justcards.commons.AppError._
import org.justcards.commons.{AppError, GameId, LobbyId, UserId}

trait View {

  /**
   * Show error
   */
  def error(error: AppError.Value): Unit

  /**
   * Ask to user the nickname
   */
  def chooseNickname(): Unit

  /**
   * Show menu
   */
  def showMenu(): Unit

  /**
   * Initialize lobby creation
   * @param games available games
   */
  def showLobbyCreation(games: Set[GameId]): Unit

  /**
   * Show available lobbies
   * @param lobbies available lobbies
   */
  def showLobbyJoin(lobbies: Set[(LobbyId, Set[UserId])]): Unit

  def lobbyCreated(lobby: LobbyId): Unit

  def lobbyJoined(lobby: LobbyId, members: Set[UserId]): Unit

  def lobbyUpdate(lobby: LobbyId, members: Set[UserId]): Unit
}

trait ViewFactory extends (AppController => View)

object View {

  case class ShowError(error: AppError.Value)
  case object ChooseNickname
  case object ShowMenu
  case class ShowLobbyCreation(games: Set[GameId])
  case class ShowLobbies(lobbies: Set[(LobbyId, Set[UserId])])
  case class LobbyCreated(lobby: LobbyId)
  case class LobbyJoined(lobby: LobbyId, members: Set[UserId])
  case class LobbyUpdate(lobby: LobbyId, members: Set[UserId])

  val MENU_TITLE = "MENU"
  val LOBBY_CREATION_TITLE = "LOBBY CREATION - Choose the game you want to play"
  val LOBBY_LIST_TITLE = "LOBBY LIST - Choose the lobby you want to join"
  val CHOOSE_NICKNAME = "Choose your nickname:"
  val LOBBY_MESSAGE = "If you want to exit from the lobby, write \"exit\""
  val EXIT = "exit"

  def LOBBY_CREATED_MESSAGE(lobby: LobbyId): String = "Your lobby has been created and its ID is " + lobby.id
  def LOBBY_JOINED_MESSAGE(lobby: LobbyId): String = "You joined to lobby " + lobby.id
  def UNKNOWN_ERROR(error: AppError.Value): String = "Unknown error: " + error

  val ERROR_CONNECTION_LOST = "Error: Connection Lost"
  val ERROR_CANNOT_CONNECT = "Error: I can't connect"
  val ERROR_LAST_MESSAGE_LOST = "Error: Last message didn't arrive"

  val ERROR_USERNAME_ALREADY_USED = "Error: Username already used"
  val ERROR_USER_NOT_LOGGED = "Error: You're not logged"
  val ERROR_USER_ALREADY_LOGGED = "Error: You're already logged"
  val ERROR_USER_ALREADY_IN_LOBBY = "Error: You're already in a lobby"
  val ERROR_USER_WRONG_USERNAME = "Error: Action not allowed with this username"

  val ERROR_GAME_NOT_EXIST = "Error: Selected game doesn't exist"
  val ERROR_LOBBY_NOT_EXIST = "Error: Selected lobby doesn't exist"
  val ERROR_LOBBY_FULL = "Error: Selected lobby is already full"

  val ERROR_WRONG_CHOICE = "Error: The selected choice is not available"

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
