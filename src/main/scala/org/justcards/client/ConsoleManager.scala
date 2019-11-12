package org.justcards.client

import org.justcards.commons.GameId

trait ConsoleManager extends Thread {

  /**
   * Ask to user the nickname
   */
  def chooseNickname(): Unit

  /**
   * Error in login phase (probably nickname is already taken)
   */
  def errorLogin(): Unit

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
  def showLobbyJoining(lobbies: List[Lobby]): Unit
}

object ConsoleManager {
  val MENU_TITLE = "MENU"
  val LOBBY_CREATION_TITLE = "LOBBY CREATION - Choose the game you want to play"
  val LOBBY_LIST_TITLE = "LOBBY LIST - Choose the lobby you want to join"
  val CHOOSE_NICKNAME = "Choose your nickname:"
  val NICKNAME_ERROR = "Error: Nickname already used"
}
