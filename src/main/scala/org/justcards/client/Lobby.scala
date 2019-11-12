package org.justcards.client

import org.justcards.commons.{GameId, LobbyId}

case class Lobby(id: LobbyId, game: GameId, creator: String, participants: Int) {
  override def toString: String = "#" + id.id + " - " + game.name + " - "+ creator + " (" + participants + "/4 participants)"
}
