package org.justcards.client

import org.justcards.commons.GameId

trait AppController {

  def login(nickname: String): Unit
  def menuSelection(choice: MenuChoice.Value): Unit
  def createLobby(game: GameId): Unit
  def joinToLobby(lobby: Lobby): Unit

}

case class AppControllerImpl() extends AppController {
  override def login(nickname: String): Unit = println("Controller " + nickname)

  override def menuSelection(choice: MenuChoice.Value): Unit = println("Controller " + choice)

  override def createLobby(game: GameId): Unit = println("Controller " + game)

  override def joinToLobby(lobby: Lobby): Unit = println("Controller " + lobby)
}