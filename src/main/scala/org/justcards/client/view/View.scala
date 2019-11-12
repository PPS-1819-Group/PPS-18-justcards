package org.justcards.client.view

import org.justcards.client.controller.AppController
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

object ConsoleManager {
  val MENU_TITLE = "MENU"
  val LOBBY_CREATION_TITLE = "LOBBY CREATION - Choose the game you want to play"
  val LOBBY_LIST_TITLE = "LOBBY LIST - Choose the lobby you want to join"
  val CHOOSE_NICKNAME = "Choose your nickname:"
  val NICKNAME_ERROR = "Error: Nickname already used"
  val LAST_MESSAGE_LOST = "Last message didn't arrive"
  val CONNECTION_LOST = "Connection Lost"
  val CANNOT_CONNECT = "I can't connect"
}
