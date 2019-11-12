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
   * Show menu and let user decide
   */
  def showMenu(): Unit

  /**
   *
   */
  def showLobbyCreation(games: Set[GameId]): Unit

  /**
   *
   */
  def showLobbyJoining(games: List[Lobby]): Unit
}
