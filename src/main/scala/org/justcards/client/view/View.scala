package org.justcards.client.view

import org.justcards.client.controller.AppController
import org.justcards.commons.{GameId, LobbyId, UserId}

trait View {
  def error(message: String): Unit
  def logged(): Unit
  def showLobbyCreation(games: Set[GameId]): Unit
  def lobbyCreated(lobby: LobbyId): Unit
  def showLobbyJoin(lobbies: Set[LobbyId]): Unit
  def lobbyJoined(lobby: LobbyId, members: Set[UserId]): Unit
}

trait ViewFactory extends (AppController => View)