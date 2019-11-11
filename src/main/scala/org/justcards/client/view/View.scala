package org.justcards.client.view

import org.justcards.client.controller.AppController
import org.justcards.commons.{GameId, LobbyId}

trait View {
  def error(message: String): Unit
  def logged(): Unit
  def showLobbyCreation(games: Set[GameId]): Unit
  def lobbyCreated(lobby: LobbyId): Unit
}

trait ViewFactory extends (AppController => View)